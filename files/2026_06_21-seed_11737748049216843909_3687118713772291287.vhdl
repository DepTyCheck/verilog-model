-- Seed: 11737748049216843909,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity fthikyp is
  port (sepg : inout time; vjj : inout std_logic; vsdapd : in time; rlltbtzb : inout std_logic_vector(4 downto 0));
end fthikyp;

architecture jrfuubmo of fthikyp is
  
begin
  -- Multi-driven assignments
  rlltbtzb <= ('Z', '0', 'W', 'W', '-');
end jrfuubmo;

entity oza is
  port (baf : in real_vector(0 downto 4));
end oza;

library ieee;
use ieee.std_logic_1164.all;

architecture liuldc of oza is
  signal ewpujkv : std_logic_vector(4 downto 0);
  signal ydeuu : time;
  signal sbakxfep : time;
  signal qnrubsd : std_logic_vector(4 downto 0);
  signal wlhhpvn : time;
  signal ivt : std_logic;
  signal rhukemm : time;
  signal bf : time;
  signal pzrjtadh : std_logic;
  signal ykipyky : std_logic_vector(4 downto 0);
  signal u : time;
  signal vtpt : std_logic;
  signal pr : time;
begin
  rztsbtm : entity work.fthikyp
    port map (sepg => pr, vjj => vtpt, vsdapd => u, rlltbtzb => ykipyky);
  hjohze : entity work.fthikyp
    port map (sepg => u, vjj => pzrjtadh, vsdapd => bf, rlltbtzb => ykipyky);
  ixg : entity work.fthikyp
    port map (sepg => rhukemm, vjj => ivt, vsdapd => wlhhpvn, rlltbtzb => qnrubsd);
  zisj : entity work.fthikyp
    port map (sepg => sbakxfep, vjj => vtpt, vsdapd => ydeuu, rlltbtzb => ewpujkv);
end liuldc;



-- Seed after: 13499638479228791125,3687118713772291287
