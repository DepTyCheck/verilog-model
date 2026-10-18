-- Seed: 17127208520557997421,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity wps is
  port (wpwglag : buffer std_logic; l : inout std_logic; szyocd : inout std_logic);
end wps;

architecture t of wps is
  
begin
  -- Multi-driven assignments
  wpwglag <= 'U';
  szyocd <= szyocd;
  wpwglag <= 'H';
  szyocd <= szyocd;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity oabtrikrcc is
  port (jvczfiok : out real; c : linkage std_logic_vector(4 to 2); ga : buffer real);
end oabtrikrcc;

library ieee;
use ieee.std_logic_1164.all;

architecture fofqczjlt of oabtrikrcc is
  signal mxri : std_logic;
  signal a : std_logic;
  signal pia : std_logic;
begin
  m : entity work.wps
    port map (wpwglag => pia, l => pia, szyocd => a);
  wunrhhvs : entity work.wps
    port map (wpwglag => pia, l => mxri, szyocd => pia);
  
  -- Multi-driven assignments
  mxri <= '0';
  pia <= 'X';
  pia <= '1';
  a <= pia;
end fofqczjlt;

entity bzgegmt is
  port (iyfjtgwmo : linkage time; ulkklw : linkage bit_vector(1 downto 2));
end bzgegmt;

library ieee;
use ieee.std_logic_1164.all;

architecture wz of bzgegmt is
  signal bhmh : std_logic;
  signal nqtv : std_logic;
  signal ijdxfwdgq : std_logic;
begin
  ztdgz : entity work.wps
    port map (wpwglag => ijdxfwdgq, l => nqtv, szyocd => bhmh);
end wz;

library ieee;
use ieee.std_logic_1164.all;

entity ecrvnovx is
  port (atnnoqs : in bit; wcmmwngvvn : linkage std_logic);
end ecrvnovx;

library ieee;
use ieee.std_logic_1164.all;

architecture ltnyekdh of ecrvnovx is
  signal siup : std_logic;
  signal qdwb : std_logic;
  signal secrfn : std_logic;
  signal ds : std_logic;
begin
  ygmoh : entity work.wps
    port map (wpwglag => ds, l => secrfn, szyocd => ds);
  mky : entity work.wps
    port map (wpwglag => ds, l => qdwb, szyocd => siup);
end ltnyekdh;



-- Seed after: 12112350784883017917,14108379317686958507
