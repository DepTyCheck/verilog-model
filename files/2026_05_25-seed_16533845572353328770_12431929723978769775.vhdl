-- Seed: 16533845572353328770,12431929723978769775

library ieee;
use ieee.std_logic_1164.all;

entity coh is
  port (qrxcmg : in std_logic);
end coh;



architecture lvlc of coh is
  
begin
  
end lvlc;

library ieee;
use ieee.std_logic_1164.all;

entity wfrffnxol is
  port (xwegjg : buffer real; vybw : inout time; wiwljcsnyk : inout std_logic; gponn : in real);
end wfrffnxol;



architecture zlvnbwyt of wfrffnxol is
  
begin
  b : entity work.coh
    port map (qrxcmg => wiwljcsnyk);
end zlvnbwyt;



entity jkbvlbs is
  port (nto : out severity_level);
end jkbvlbs;

library ieee;
use ieee.std_logic_1164.all;

architecture px of jkbvlbs is
  signal r : std_logic;
  signal ddrg : time;
  signal nxk : real;
  signal alfg : std_logic;
  signal qkfuxpjcf : time;
  signal brlfhzxhd : real;
begin
  h : entity work.wfrffnxol
    port map (xwegjg => brlfhzxhd, vybw => qkfuxpjcf, wiwljcsnyk => alfg, gponn => nxk);
  viybgh : entity work.wfrffnxol
    port map (xwegjg => nxk, vybw => ddrg, wiwljcsnyk => r, gponn => brlfhzxhd);
  yohwt : entity work.coh
    port map (qrxcmg => r);
  wgau : entity work.coh
    port map (qrxcmg => alfg);
end px;

library ieee;
use ieee.std_logic_1164.all;

entity ekzql is
  port (r : buffer std_logic_vector(2 to 1); jnqcqpv : out real; aeumkfynga : out character);
end ekzql;

library ieee;
use ieee.std_logic_1164.all;

architecture ka of ekzql is
  signal jppqzx : std_logic;
  signal utd : std_logic;
begin
  rsuklpk : entity work.coh
    port map (qrxcmg => utd);
  uiqxxtubvf : entity work.coh
    port map (qrxcmg => jppqzx);
  wlsfihsu : entity work.coh
    port map (qrxcmg => utd);
end ka;



-- Seed after: 17103173811344849879,12431929723978769775
