-- Seed: 10254408138958797851,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity qcjr is
  port (obz : buffer integer; ssostvu : inout integer; ie : linkage std_logic_vector(0 downto 1));
end qcjr;

architecture wnbtukqz of qcjr is
  
begin
  
end wnbtukqz;

entity fs is
  port (llwsz : linkage boolean_vector(4 downto 4));
end fs;

library ieee;
use ieee.std_logic_1164.all;

architecture gdhbhjm of fs is
  signal yk : std_logic_vector(0 downto 1);
  signal rsve : integer;
  signal slmsxethhb : integer;
begin
  ritlytwd : entity work.qcjr
    port map (obz => slmsxethhb, ssostvu => rsve, ie => yk);
  
  -- Multi-driven assignments
  yk <= (others => '0');
  yk <= (others => '0');
  yk <= "";
end gdhbhjm;

library ieee;
use ieee.std_logic_1164.all;

entity qjcvbamc is
  port (rvfqpp : inout std_logic_vector(4 to 4));
end qjcvbamc;

library ieee;
use ieee.std_logic_1164.all;

architecture xroceso of qjcvbamc is
  signal i : std_logic_vector(0 downto 1);
  signal zq : integer;
  signal uuwp : integer;
  signal dgghbyalqy : boolean_vector(4 downto 4);
  signal zjqtrlsvfl : integer;
  signal khx : integer;
  signal fxn : std_logic_vector(0 downto 1);
  signal btgigwodj : integer;
  signal qiurbmsuur : integer;
begin
  ubv : entity work.qcjr
    port map (obz => qiurbmsuur, ssostvu => btgigwodj, ie => fxn);
  het : entity work.qcjr
    port map (obz => khx, ssostvu => zjqtrlsvfl, ie => fxn);
  tzfo : entity work.fs
    port map (llwsz => dgghbyalqy);
  ihn : entity work.qcjr
    port map (obz => uuwp, ssostvu => zq, ie => i);
  
  -- Multi-driven assignments
  fxn <= (others => '0');
  rvfqpp <= (others => 'L');
  rvfqpp <= (others => '1');
end xroceso;



-- Seed after: 12826963152198734182,12011142928354116943
