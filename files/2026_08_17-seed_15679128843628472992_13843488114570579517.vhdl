-- Seed: 15679128843628472992,13843488114570579517

entity titst is
  port (zsdr : in real; kbsyb : out time; v : buffer real);
end titst;

architecture om of titst is
  
begin
  -- Single-driven assignments
  v <= zsdr;
  kbsyb <= 3 min;
end om;

library ieee;
use ieee.std_logic_1164.all;

entity mmtivddg is
  port (xicfoy : in std_logic_vector(3 downto 0));
end mmtivddg;

architecture flthzgmtdk of mmtivddg is
  signal ejwnh : real;
  signal nic : time;
  signal qnn : real;
  signal iqvrd : time;
  signal d : time;
  signal iogqlaysp : real;
  signal ztqlhnem : real;
  signal wvieghoy : time;
  signal wcsriajxx : real;
begin
  ih : entity work.titst
    port map (zsdr => wcsriajxx, kbsyb => wvieghoy, v => ztqlhnem);
  yryfqfk : entity work.titst
    port map (zsdr => iogqlaysp, kbsyb => d, v => iogqlaysp);
  rqcl : entity work.titst
    port map (zsdr => iogqlaysp, kbsyb => iqvrd, v => wcsriajxx);
  ddtstns : entity work.titst
    port map (zsdr => qnn, kbsyb => nic, v => ejwnh);
  
  -- Single-driven assignments
  qnn <= 16#F7.0_7_0_3#;
end flthzgmtdk;



-- Seed after: 17005311571566916785,13843488114570579517
