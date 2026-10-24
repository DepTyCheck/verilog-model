-- Seed: 15539574015765480417,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (iajfkodr : buffer time; bnylx : out real; yoajrjiz : out std_logic);
end s;

architecture hosgauf of s is
  
begin
  -- Single-driven assignments
  bnylx <= bnylx;
  iajfkodr <= iajfkodr;
  
  -- Multi-driven assignments
  yoajrjiz <= 'L';
  yoajrjiz <= 'W';
  yoajrjiz <= '-';
  yoajrjiz <= yoajrjiz;
end hosgauf;

library ieee;
use ieee.std_logic_1164.all;

entity rermcj is
  port (itgpbbi : inout real_vector(1 to 1); vkqfozn : buffer std_logic; qabsnut : linkage integer_vector(1 downto 2));
end rermcj;

library ieee;
use ieee.std_logic_1164.all;

architecture ilodk of rermcj is
  signal ztrhglvif : real;
  signal oak : time;
  signal rzuydbe : std_logic;
  signal zvrygkw : real;
  signal svmpud : time;
  signal yexhokvy : std_logic;
  signal iptcvc : real;
  signal vflknfnnb : time;
begin
  nd : entity work.s
    port map (iajfkodr => vflknfnnb, bnylx => iptcvc, yoajrjiz => yexhokvy);
  fkokawm : entity work.s
    port map (iajfkodr => svmpud, bnylx => zvrygkw, yoajrjiz => rzuydbe);
  rgj : entity work.s
    port map (iajfkodr => oak, bnylx => ztrhglvif, yoajrjiz => vkqfozn);
  
  -- Multi-driven assignments
  yexhokvy <= 'X';
  vkqfozn <= vkqfozn;
end ilodk;

library ieee;
use ieee.std_logic_1164.all;

entity mfdcf is
  port (anziso : inout real; p : linkage boolean_vector(4 to 3); upoy : out std_logic_vector(1 to 0));
end mfdcf;

library ieee;
use ieee.std_logic_1164.all;

architecture zvy of mfdcf is
  signal naxpshm : integer_vector(1 downto 2);
  signal sv : std_logic;
  signal e : real_vector(1 to 1);
  signal gawee : std_logic;
  signal zmmdr : time;
  signal mxwzprymqo : std_logic;
  signal rudbs : real;
  signal yuwoyypef : time;
begin
  qasw : entity work.s
    port map (iajfkodr => yuwoyypef, bnylx => rudbs, yoajrjiz => mxwzprymqo);
  yzmg : entity work.s
    port map (iajfkodr => zmmdr, bnylx => anziso, yoajrjiz => gawee);
  vmu : entity work.rermcj
    port map (itgpbbi => e, vkqfozn => sv, qabsnut => naxpshm);
  
  -- Multi-driven assignments
  upoy <= "";
end zvy;



-- Seed after: 14510910857235270268,10975587770517081373
