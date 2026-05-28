-- Seed: 3578002589888283946,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity namvtya is
  port (mwdpnxg : out time_vector(1 downto 2); x : inout std_logic_vector(0 to 3));
end namvtya;



architecture lte of namvtya is
  
begin
  
end lte;

library ieee;
use ieee.std_logic_1164.all;

entity abvyf is
  port (n : buffer real; meqxqlwq : buffer std_logic; ymbz : out real_vector(3 downto 1); zvj : buffer boolean);
end abvyf;

library ieee;
use ieee.std_logic_1164.all;

architecture ksxcvhcz of abvyf is
  signal djihrdhqtw : time_vector(1 downto 2);
  signal whdlqvvfa : std_logic_vector(0 to 3);
  signal bfaxswlbtj : time_vector(1 downto 2);
  signal fy : std_logic_vector(0 to 3);
  signal vkizfygc : time_vector(1 downto 2);
begin
  mesvkai : entity work.namvtya
    port map (mwdpnxg => vkizfygc, x => fy);
  jvkwtp : entity work.namvtya
    port map (mwdpnxg => bfaxswlbtj, x => whdlqvvfa);
  ntblnchp : entity work.namvtya
    port map (mwdpnxg => djihrdhqtw, x => fy);
end ksxcvhcz;



entity u is
  port (k : linkage real; ckpgq : buffer integer; t : in real);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture dwu of u is
  signal x : boolean;
  signal fjkgz : real_vector(3 downto 1);
  signal nuqjrohvc : std_logic;
  signal o : real;
begin
  tnaj : entity work.abvyf
    port map (n => o, meqxqlwq => nuqjrohvc, ymbz => fjkgz, zvj => x);
end dwu;

library ieee;
use ieee.std_logic_1164.all;

entity fraigk is
  port (gqod : out std_logic);
end fraigk;

library ieee;
use ieee.std_logic_1164.all;

architecture eyl of fraigk is
  signal jptw : time_vector(1 downto 2);
  signal grpu : std_logic_vector(0 to 3);
  signal qixqfnmv : time_vector(1 downto 2);
  signal zrokvapusi : std_logic_vector(0 to 3);
  signal zxkqp : time_vector(1 downto 2);
begin
  dazxws : entity work.namvtya
    port map (mwdpnxg => zxkqp, x => zrokvapusi);
  bfrrltfzn : entity work.namvtya
    port map (mwdpnxg => qixqfnmv, x => grpu);
  ztchlfu : entity work.namvtya
    port map (mwdpnxg => jptw, x => grpu);
end eyl;



-- Seed after: 14254842809468990246,6329330932550885447
