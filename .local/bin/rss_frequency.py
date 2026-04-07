#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["feedparser", "python-dateutil"]
# ///
"""
RSS Feed Frequency Analyzer
============================
Given a list of feed URLs (plain text or OPML), fetches each one, computes
posting frequency, and groups feeds into categories suitable for folder
organization. Supports RSS, Atom, and JSON Feed formats.

Recommended usage — export from your reader, sort, re-import:
    uv run rss_frequency.py feeds.opml --output-opml sorted.opml

Other options:
    uv run rss_frequency.py feeds.txt          # plain text, one URL per line
    uv run rss_frequency.py feeds.opml         # just print the report
    uv run rss_frequency.py feeds.txt --format text
    uv run rss_frequency.py feeds.opml --format opml
    uv run rss_frequency.py feeds.txt --weeks 8
    uv run rss_frequency.py feeds.txt --json
    uv run rss_frequency.py feeds.txt --output-opml sorted.opml --json > data.json
    echo "https://example.com/feed.json" | uv run rss_frequency.py -

Plain text format: one URL per line, # lines are comments, blank lines ignored.
OPML format: standard OPML 1.0/2.0 subscription lists (e.g. exported from
             NetNewsWire, Reeder, Feedly, Inoreader, etc.)
"""

import argparse
import json
import sys
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
from collections import defaultdict
from datetime import datetime, timezone

import feedparser
from dateutil import parser as dateparser

# ── Frequency buckets (posts/week thresholds) ────────────────────────────────
BUCKETS = [
    ("Multiple times a day", 7.0),  # > 7/week
    ("Daily", 4.0),  # 4–7/week
    ("A few times a week", 1.5),  # 1.5–4/week
    ("Weekly", 0.75),  # 0.75–1.5/week  (~once a week)
    ("A few times a month", 0.25),  # 0.25–0.75/week (~biweekly/triweekly)
    ("Monthly", 0.10),  # 0.10–0.25/week
    ("Rarely", 0.0),  # anything lower
]

JSON_FEED_CONTENT_TYPES = {
    "application/feed+json",
    "application/json",
    "text/json",
}

USER_AGENT = "rss-frequency-analyzer/1.0 (feed reader)"


def bucket_for(posts_per_week: float) -> str:
    for label, threshold in BUCKETS:
        if posts_per_week > threshold:
            return label
    return "Rarely"


# ── Date parsing ─────────────────────────────────────────────────────────────


def parse_date_feedparser(entry) -> datetime | None:
    """Extract a tz-aware datetime from a feedparser entry object."""
    for field in ("published_parsed", "updated_parsed"):
        t = getattr(entry, field, None)
        if t:
            try:
                import time as _time

                return datetime.fromtimestamp(_time.mktime(t), tz=timezone.utc)
            except Exception:
                pass
    for field in ("published", "updated"):
        raw = getattr(entry, field, None)
        if raw:
            try:
                return dateparser.parse(raw).astimezone(timezone.utc)
            except Exception:
                pass
    return None


def parse_date_str(raw: str | None) -> datetime | None:
    """Parse an ISO-8601 or RFC-2822 date string into a tz-aware datetime."""
    if not raw:
        return None
    try:
        return dateparser.parse(raw).astimezone(timezone.utc)
    except Exception:
        return None


# ── Feed fetching & parsing ───────────────────────────────────────────────────


def is_json_content_type(content_type: str) -> bool:
    base = content_type.split(";")[0].strip().lower()
    return base in JSON_FEED_CONTENT_TYPES


def fetch_url(url: str) -> tuple[bytes, str]:
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(req, timeout=15) as resp:
        return resp.read(), resp.headers.get("Content-Type", "")


def dates_from_json_feed(data: dict) -> tuple[str | None, list[datetime]]:
    """Parse a JSON Feed dict; return (title, list of datetimes)."""
    title = data.get("title")
    dates = []
    for item in data.get("items", []):
        d = parse_date_str(item.get("date_published") or item.get("date_modified"))
        if d:
            dates.append(d)
    return title, dates


def dates_from_xml_feed(body: bytes) -> tuple[str | None, list[datetime]]:
    """Parse an RSS/Atom feed with feedparser; return (title, list of datetimes)."""
    feed = feedparser.parse(body)
    if feed.bozo and not feed.entries:
        raise ValueError(f"Parse error: {feed.bozo_exception}")
    title = feed.feed.get("title")
    dates = [d for entry in feed.entries if (d := parse_date_feedparser(entry))]
    return title, dates


def fetch_dates(url: str) -> tuple[str | None, list[datetime]]:
    """
    Fetch a feed URL and return (title, dates).
    Detection order:
      1. Content-Type is a known JSON type  → try JSON Feed parser
      2. URL ends with .json or body starts with '{'  → try JSON Feed parser
      3. Fall through to feedparser (RSS / Atom)
    """
    body, content_type = fetch_url(url)

    # 1. Explicit JSON content type
    if is_json_content_type(content_type):
        try:
            data = json.loads(body)
            if "items" in data:  # looks like a JSON Feed
                return dates_from_json_feed(data)
        except json.JSONDecodeError:
            pass  # fall through to feedparser

    # 2. URL hint or body starts with '{'
    if url.lower().endswith(".json") or body.lstrip()[:1] == b"{":
        try:
            data = json.loads(body)
            if "items" in data:
                return dates_from_json_feed(data)
        except json.JSONDecodeError:
            pass

    # 3. RSS / Atom
    return dates_from_xml_feed(body)


# ── Core analysis ─────────────────────────────────────────────────────────────


def compute_frequency(dates: list[datetime], window_weeks: int, now: datetime) -> dict:
    window_days = window_weeks * 7
    in_window = [d for d in dates if (now - d).days <= window_days]

    if in_window:
        oldest = in_window[-1]
        days_covered = max((now - oldest).days, 1)
        return {
            "posts_in_window": len(in_window),
            "oldest_post_days_ago": (now - oldest).days,
            "posts_per_week": len(in_window) / (days_covered / 7),
        }
    elif dates:
        # Feed hasn't posted in the window — rate based on all available data
        oldest = dates[-1]
        days_covered = max((now - oldest).days, 1)
        return {
            "posts_in_window": 0,
            "oldest_post_days_ago": (now - dates[0]).days,
            "posts_per_week": len(dates) / (days_covered / 7),
        }
    return {"posts_in_window": 0, "oldest_post_days_ago": None, "posts_per_week": 0.0}


def analyze_feed(url: str, window_weeks: int) -> dict:
    result = {
        "url": url,
        "title": None,
        "posts_per_week": None,
        "posts_in_window": None,
        "oldest_post_days_ago": None,
        "bucket": "Unknown",
        "error": None,
    }

    try:
        title, dates = fetch_dates(url)
    except urllib.error.URLError as e:
        result["error"] = f"Network error: {e.reason}"
        return result
    except Exception as e:
        result["error"] = str(e)
        return result

    result["title"] = title or url

    if not dates:
        result["error"] = "No parseable dates in entries"
        return result

    dates.sort(reverse=True)
    now = datetime.now(tz=timezone.utc)
    result.update(compute_frequency(dates, window_weeks, now))
    result["bucket"] = bucket_for(result["posts_per_week"] or 0.0)
    return result


# ── Input loading ─────────────────────────────────────────────────────────────


def load_urls_text(source: str) -> list[str]:
    if source == "-":
        lines = sys.stdin.read().splitlines()
    else:
        with open(source) as f:
            lines = f.read().splitlines()
    return [l.strip() for l in lines if l.strip() and not l.strip().startswith("#")]


def load_urls_opml(source: str) -> list[str]:
    if source == "-":
        root = ET.fromstring(sys.stdin.buffer.read())
    else:
        root = ET.parse(source).getroot()

    urls = []

    def walk(node):
        for outline in node.findall("outline"):
            url = outline.get("xmlUrl") or outline.get("xmlurl")
            if url:
                urls.append(url.strip())
            walk(outline)

    body = root.find("body")
    if body is None:
        raise ValueError("OPML file has no <body> element")
    walk(body)
    return urls


def detect_format(source: str) -> str:
    if source != "-" and source.lower().endswith(".opml"):
        return "opml"
    return "text"


def load_urls(source: str, fmt: str | None) -> list[str]:
    resolved = fmt or detect_format(source)
    if resolved == "opml":
        return load_urls_opml(source)
    return load_urls_text(source)


# ── Output ────────────────────────────────────────────────────────────────────


def print_results(results: list[dict], window_weeks: int):
    grouped = defaultdict(list)
    errors = []

    for r in results:
        if r["error"]:
            errors.append(r)
        else:
            grouped[r["bucket"]].append(r)

    print(f"\n{'═' * 60}")
    print(f"  RSS FEED FREQUENCY REPORT  (analysis window: {window_weeks} weeks)")
    print(f"{'═' * 60}\n")

    for label, _ in BUCKETS:
        feeds = grouped.get(label, [])
        if not feeds:
            continue
        feeds.sort(key=lambda r: r["posts_per_week"] or 0, reverse=True)
        print(
            f"📁  {label.upper()}  ({len(feeds)} feed{'s' if len(feeds) > 1 else ''})"
        )
        print(f"{'─' * 60}")
        for r in feeds:
            ppw = r["posts_per_week"]
            title = (r["title"] or r["url"])[:45]
            freq_str = f"{ppw:.1f}/wk" if ppw is not None else "?"
            in_win = r["posts_in_window"]
            print(f"  {title:<46} {freq_str:>8}  ({in_win} posts in window)")
            print(f"    {r['url']}")
        print()

    if errors:
        print(f"⚠️   ERRORS  ({len(errors)} feed{'s' if len(errors) > 1 else ''})")
        print(f"{'─' * 60}")
        for r in errors:
            print(f"  {r['url']}")
            print(f"    → {r['error']}")
        print()

    total = len(results)
    ok = total - len(errors)
    print(f"{'─' * 60}")
    print(f"  Total: {total} feeds — {ok} analyzed, {len(errors)} failed")
    print(f"{'═' * 60}\n")


def write_opml(results: list[dict], path: str, window_weeks: int):
    """Write an OPML file with one folder per frequency bucket."""
    grouped = defaultdict(list)
    for r in results:
        if not r["error"]:
            grouped[r["bucket"]].append(r)

    now_str = datetime.now(tz=timezone.utc).strftime("%a, %d %b %Y %H:%M:%S GMT")

    opml = ET.Element("opml", version="2.0")

    head = ET.SubElement(opml, "head")
    ET.SubElement(head, "title").text = "Feeds by Posting Frequency"
    ET.SubElement(head, "dateCreated").text = now_str
    ET.SubElement(head, "docs").text = "http://opml.org/spec2.opml"

    body = ET.SubElement(opml, "body")

    for label, _ in BUCKETS:
        feeds = grouped.get(label, [])
        if not feeds:
            continue
        feeds.sort(key=lambda r: r["posts_per_week"] or 0, reverse=True)

        folder = ET.SubElement(body, "outline", text=label, title=label)
        for r in feeds:
            attrs = {
                "type": "rss",
                "text": r["title"] or r["url"],
                "title": r["title"] or r["url"],
                "xmlUrl": r["url"],
            }
            ET.SubElement(folder, "outline", **attrs)

    # Feeds that errored go into a catch-all so nothing is silently lost
    errors = [r for r in results if r["error"]]
    if errors:
        folder = ET.SubElement(
            body, "outline", text="⚠️ Unanalyzed", title="⚠️ Unanalyzed"
        )
        for r in errors:
            attrs = {
                "type": "rss",
                "text": r["url"],
                "title": r["url"],
                "xmlUrl": r["url"],
            }
            ET.SubElement(folder, "outline", **attrs)

    ET.indent(opml, space="  ")
    tree = ET.ElementTree(opml)
    with open(path, "wb") as f:
        f.write(b'<?xml version="1.0" encoding="UTF-8"?>\n')
        tree.write(f, encoding="utf-8", xml_declaration=False)

    print(
        f"OPML written to {path}  ({len(results) - len(errors)} feeds in {len(grouped)} folders)",
        file=sys.stderr,
    )


# ── Entry point ───────────────────────────────────────────────────────────────


def main():
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    ap.add_argument("feeds", help="Path to file with feed URLs, or - for stdin")
    ap.add_argument(
        "--format",
        choices=["text", "opml"],
        default=None,
        metavar="FMT",
        help="Input format: 'text' (one URL per line) or 'opml'. "
        "Auto-detected from .opml extension if omitted.",
    )
    ap.add_argument(
        "--weeks",
        type=int,
        default=12,
        metavar="N",
        help="Analysis window in weeks (default: 12)",
    )
    ap.add_argument(
        "--json",
        action="store_true",
        help="Output raw JSON instead of formatted report",
    )
    ap.add_argument(
        "--output-opml",
        metavar="FILE",
        help="Write results as an OPML file with one folder per frequency bucket",
    )
    ap.add_argument(
        "--concurrency",
        type=int,
        default=10,
        metavar="N",
        help="Number of feeds to fetch in parallel (default: 10)",
    )
    args = ap.parse_args()

    urls = load_urls(args.feeds, args.format)
    if not urls:
        print("No URLs found.", file=sys.stderr)
        sys.exit(1)

    print(
        f"Fetching {len(urls)} feeds (window: {args.weeks} weeks)...", file=sys.stderr
    )

    from concurrent.futures import ThreadPoolExecutor, as_completed

    results = []
    with ThreadPoolExecutor(max_workers=args.concurrency) as pool:
        futures = {pool.submit(analyze_feed, url, args.weeks): url for url in urls}
        done = 0
        for future in as_completed(futures):
            done += 1
            r = future.result()
            results.append(r)
            status = r["title"] or r["url"]
            tag = f"✓ {r['bucket']}" if not r["error"] else f"✗ {r['error']}"
            print(f"  [{done}/{len(urls)}] {status[:50]}  {tag}", file=sys.stderr)

    if args.json:
        print(json.dumps(results, indent=2, default=str))
    else:
        print_results(results, args.weeks)

    if args.output_opml:
        write_opml(results, args.output_opml, args.weeks)


if __name__ == "__main__":
    main()
