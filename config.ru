use Rack::Static

run lambda { |env|
  [
    200, 
    {
      'Content-Type'  => 'text/html', 
      'Cache-Control' => 'max-age=86400' 
    },
    File.open('index.html', File::RDONLY)
  ]
}