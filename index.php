<?php

require_once '../conf/meetup-api-key.conf.php';

function fetchUrl($url)
{
  $ch = curl_init();
  curl_setopt($ch, CURLOPT_URL, $url);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
  $content = curl_exec($ch);
  curl_close($ch);
  return $content;
}

function createMeetupUrl($meetupApiKey) {
  return 'https://api.meetup.com/2/events?&sign=true&group_urlname=seahug&status=upcoming&page=1&key=' . $meetupApiKey;
}

$content = fetchUrl(createMeetupUrl($meetupApiKey));
$obj = json_decode($content);

$event = count($obj->results) > 0 ? $obj->results[0] : null;

if ($event)
{
  $date = DateTime::createFromFormat('U', ($event->time + $event->utc_offset) / 1000);
  $title = $event->name;
  $url = $event->event_url;
}

?><!DOCTYPE html>
<html>
<head>
  <title>seattlehaskell.org</title>
</head>
<body>
<h1><tt>seattlehaskell.org</tt></h1>
<p>
  This is the future home of the Seattle Area Haskell Users' Group (SeaHUG).
</p>
<p>
<? if ($event): ?>
  <a href="<?= htmlspecialchars($url) ?>">Next event: <?= htmlspecialchars($title) ?> at <?= $date->format('Y-m-d H:i:s') ?></a>
<? else: ?>
  We have no upcoming events.
<? endif; ?>
</p>
<p>
  In the meantime, visit our <a href="http://www.meetup.com/SEAHUG/">Meetup.com</a> web site.
</p>
</body>
</html>
