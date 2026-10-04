-- Seed: 11798144479492089790,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity ui is
  port (ggqwkks : linkage std_logic_vector(4 to 3); upsqxb : linkage integer_vector(0 to 4); aqouvww : inout bit);
end ui;

architecture jfv of ui is
  
begin
  -- Single-driven assignments
  aqouvww <= '1';
end jfv;

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (qgcry : inout time; hqeslnfv : linkage std_logic);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture sckka of l is
  signal iliwrioauj : bit;
  signal ohkykdiqu : integer_vector(0 to 4);
  signal cyfikxf : bit;
  signal z : integer_vector(0 to 4);
  signal gatthnfziv : bit;
  signal pmgqmsgfp : integer_vector(0 to 4);
  signal khrpihgamm : bit;
  signal ztnisrqjcv : integer_vector(0 to 4);
  signal gqsou : std_logic_vector(4 to 3);
begin
  prfnw : entity work.ui
    port map (ggqwkks => gqsou, upsqxb => ztnisrqjcv, aqouvww => khrpihgamm);
  hcir : entity work.ui
    port map (ggqwkks => gqsou, upsqxb => pmgqmsgfp, aqouvww => gatthnfziv);
  uwktp : entity work.ui
    port map (ggqwkks => gqsou, upsqxb => z, aqouvww => cyfikxf);
  udzngdwc : entity work.ui
    port map (ggqwkks => gqsou, upsqxb => ohkykdiqu, aqouvww => iliwrioauj);
  
  -- Single-driven assignments
  qgcry <= qgcry;
  
  -- Multi-driven assignments
  gqsou <= "";
end sckka;

entity xto is
  port (yeimwl : linkage time_vector(0 to 2));
end xto;

library ieee;
use ieee.std_logic_1164.all;

architecture iaq of xto is
  signal gepyuuvxa : bit;
  signal yt : integer_vector(0 to 4);
  signal kfwpk : std_logic_vector(4 to 3);
  signal myjpf : bit;
  signal ucocwwkhqs : integer_vector(0 to 4);
  signal rdhxpswi : std_logic_vector(4 to 3);
  signal de : std_logic;
  signal mswp : time;
begin
  jnish : entity work.l
    port map (qgcry => mswp, hqeslnfv => de);
  tmx : entity work.ui
    port map (ggqwkks => rdhxpswi, upsqxb => ucocwwkhqs, aqouvww => myjpf);
  vszenisbq : entity work.ui
    port map (ggqwkks => kfwpk, upsqxb => yt, aqouvww => gepyuuvxa);
  
  -- Multi-driven assignments
  kfwpk <= rdhxpswi;
end iaq;

library ieee;
use ieee.std_logic_1164.all;

entity ms is
  port (w : buffer std_logic_vector(2 downto 4); hbgci : buffer string(3 downto 5); eg : in std_logic);
end ms;

architecture h of ms is
  
begin
  -- Single-driven assignments
  hbgci <= hbgci;
end h;



-- Seed after: 1026944718722956463,15795020531041709203
