-- Seed: 307870387131156256,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity qhb is
  port (mzmjdsnwe : buffer std_logic_vector(1 to 4));
end qhb;

architecture kyvixachzl of qhb is
  
begin
  -- Multi-driven assignments
  mzmjdsnwe <= mzmjdsnwe;
end kyvixachzl;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (ydqq : buffer std_logic);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture wsgue of q is
  signal zk : std_logic_vector(1 to 4);
begin
  kgtiybvuwp : entity work.qhb
    port map (mzmjdsnwe => zk);
  jkxpmxdc : entity work.qhb
    port map (mzmjdsnwe => zk);
  cxnsxgxc : entity work.qhb
    port map (mzmjdsnwe => zk);
  
  -- Multi-driven assignments
  ydqq <= 'W';
  ydqq <= 'H';
  ydqq <= 'H';
end wsgue;

entity bsqrv is
  port (vkf : out character);
end bsqrv;

library ieee;
use ieee.std_logic_1164.all;

architecture bslzkrnhey of bsqrv is
  signal pqoe : std_logic_vector(1 to 4);
  signal yfncqhze : std_logic;
begin
  hjuhf : entity work.q
    port map (ydqq => yfncqhze);
  cdjpiguu : entity work.qhb
    port map (mzmjdsnwe => pqoe);
  forzzms : entity work.qhb
    port map (mzmjdsnwe => pqoe);
  
  -- Single-driven assignments
  vkf <= 'a';
  
  -- Multi-driven assignments
  yfncqhze <= '1';
  pqoe <= pqoe;
  pqoe <= pqoe;
end bslzkrnhey;



-- Seed after: 527600378449181044,16461708287571398341
