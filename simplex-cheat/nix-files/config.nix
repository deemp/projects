let
  # where to store config data
  dataDir = "data";
  # where to store configs under dataDir
  configsDir = "configs";
  _LOG_ENABLED = true;
  port = 8080;
  spawner = {
    file = "spawner";
    config = {
      _RETRY_TIMES = 3;
      # min time a user waits before processing a new contact
      _DELAY_CONTACT_MIN = 0.1;
      # max time a user waits before processing a new contact
      _DELAY_CONTACT_MAX = 5;
      # how long spawner waits before checking the config
      _DELAY_SPAWNER = 3;
      # is log enabled
      _LOG_ENABLED = _LOG_ENABLED;
    };
  };
  server = {
    file = "server";
    config = {
      # length of secret key
      _KEY_LENGTH = 10;
      # is log enabled
      _LOG_ENABLED = _LOG_ENABLED;
      # block size of random sequence generator
      _RANDOM_SEQ_BLOCK_SIZE = 100;
    };
  };
  contacts = {
    file = "contacts";
    config = {
      "Alice" = {
        "Bob" = { file = "alice_bob"; startsRoot = true; };
        "Tim" = { file = "alice_tim"; startsRoot = true; };
      };
      "Bob" = {
        "Kate" = { file = "kate_bob"; startsRoot = true; };
        "Tim" = { file = "bob_tim"; startsRoot = true; };
      };
    };
    # where to store chat configs
    dir = "contacts";
  };
in
{
  inherit
    dataDir
    configsDir
    spawner
    server
    contacts
    port;
}
