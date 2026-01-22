defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ]
  ]

  @any %{}
  @json %{ accept: %{ json: true } }
  @html %{ accept: %{ html: true } }

  define_layers [ :static, :services, :fall_back, :not_found ]

  #############################################################################
  # Frontend resources
  #############################################################################

  get "/organizations/*path", @json do
    forward conn, path, "http://resource/organizations/"
  end

  get "/organization-classification-codes/*path", @json do
    forward conn, path, "http://resource/organization-classification-codes/"
  end

  get "/bestuurseenheden/*path", @json do
    forward conn, path, "http://resource/bestuurseenheden/"
  end

  get "/bestuurseenheid-classificatie-codes/*path", @json do
    forward conn, path, "http://resource/bestuurseenheid-classificatie-codes/"
  end


  #############################################################################
  # Session management
  #############################################################################

  match "/mock/sessions/*path", @json do
    forward conn, path, "http://mocklogin/sessions/"
  end

  get "/gebruikers/*path", @json do
    forward conn, path, "http://resource/gebruikers/"
  end

  # NOTE: resources
  match "/accounts/*path", @json do
    forward conn, path, "http://resource/accounts/"
  end


  match "/sessions/*path" do
    forward conn, path, "http://login/sessions/"
  end

  # Frontend

  get "/assets/*path", @any do
    forward conn, path, "http://frontend/assets/"
  end

  get "/@appuniversum/*path", @any do
    forward conn, path, "http://frontend/@appuniversum/"
  end

  match "/*_path", @html do
    forward conn, [], "http://frontend/index.html"
  end

  #################################################################
  # IPDC
  #################################################################
  get "/public-services/*path" do
    Proxy.forward conn, path, "http://resource/public-services/"
  end

  get "/procedures/*path" do
    Proxy.forward conn, path, "http://resource/procedures/"
  end

  get "/websites/*path" do
    Proxy.forward conn, path, "http://resource/websites/"
  end
  #############################################################################
  # Others
  #############################################################################

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
