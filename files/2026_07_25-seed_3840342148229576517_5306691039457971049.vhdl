-- Seed: 3840342148229576517,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity dbuwk is
  port (xzqbxjf : inout std_logic_vector(4 to 3); eemjth : buffer time; zbrdrtu : inout std_logic; bqtltakp : buffer real);
end dbuwk;

architecture ikisl of dbuwk is
  
begin
  -- Single-driven assignments
  bqtltakp <= 8#5_2.2_5_3_7#;
  eemjth <= 2_2.2 ps;
end ikisl;

library ieee;
use ieee.std_logic_1164.all;

entity jefeify is
  port (h : in std_logic_vector(0 downto 2); x : buffer std_logic_vector(4 to 1); bhiabbasxp : out real; iek : in real);
end jefeify;

library ieee;
use ieee.std_logic_1164.all;

architecture ttijk of jefeify is
  signal om : real;
  signal nbco : time;
  signal njvfdsj : real;
  signal ix : time;
  signal sxxnj : time;
  signal yuvttqtv : std_logic_vector(4 to 3);
  signal sovyrknkja : real;
  signal nch : std_logic;
  signal ueiufneda : time;
begin
  yb : entity work.dbuwk
    port map (xzqbxjf => x, eemjth => ueiufneda, zbrdrtu => nch, bqtltakp => sovyrknkja);
  nbu : entity work.dbuwk
    port map (xzqbxjf => yuvttqtv, eemjth => sxxnj, zbrdrtu => nch, bqtltakp => bhiabbasxp);
  ywvyntili : entity work.dbuwk
    port map (xzqbxjf => x, eemjth => ix, zbrdrtu => nch, bqtltakp => njvfdsj);
  iefmjumg : entity work.dbuwk
    port map (xzqbxjf => yuvttqtv, eemjth => nbco, zbrdrtu => nch, bqtltakp => om);
  
  -- Multi-driven assignments
  x <= (others => '0');
  x <= x;
  x <= "";
end ttijk;



-- Seed after: 13373960593574536432,5306691039457971049
