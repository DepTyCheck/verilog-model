-- Seed: 12890529412937795227,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity bd is
  port (dbnzftl : buffer time; dwvm : buffer std_logic);
end bd;

architecture fnhls of bd is
  
begin
  -- Single-driven assignments
  dbnzftl <= 8#125.3_0_4_4_4# us;
  
  -- Multi-driven assignments
  dwvm <= '1';
  dwvm <= 'X';
  dwvm <= 'L';
  dwvm <= 'W';
end fnhls;

library ieee;
use ieee.std_logic_1164.all;

entity knlqmirtu is
  port (tawb : inout std_logic_vector(4 to 4); xzef : buffer integer; jgjzihrda : buffer time; gzgvmvewn : inout std_logic_vector(1 to 0));
end knlqmirtu;

library ieee;
use ieee.std_logic_1164.all;

architecture uhxnfnbqs of knlqmirtu is
  signal szjkxhxxq : time;
  signal mo : std_logic;
  signal qvp : time;
  signal hoffc : std_logic;
  signal rj : std_logic;
  signal y : time;
begin
  vvnbxi : entity work.bd
    port map (dbnzftl => y, dwvm => rj);
  gsxr : entity work.bd
    port map (dbnzftl => jgjzihrda, dwvm => hoffc);
  vnps : entity work.bd
    port map (dbnzftl => qvp, dwvm => mo);
  lztrqs : entity work.bd
    port map (dbnzftl => szjkxhxxq, dwvm => rj);
  
  -- Single-driven assignments
  xzef <= 0;
  
  -- Multi-driven assignments
  tawb <= "1";
  hoffc <= 'U';
end uhxnfnbqs;

library ieee;
use ieee.std_logic_1164.all;

entity xmwdf is
  port (xk : out time_vector(0 to 1); twaza : linkage std_logic_vector(3 downto 4); boc : out time);
end xmwdf;

library ieee;
use ieee.std_logic_1164.all;

architecture gmyghdkxrt of xmwdf is
  signal ywlusj : std_logic_vector(1 to 0);
  signal bijidts : integer;
  signal hmqj : std_logic_vector(4 to 4);
  signal lqoaef : std_logic;
  signal hproowf : time;
begin
  tpsmuv : entity work.bd
    port map (dbnzftl => hproowf, dwvm => lqoaef);
  qwlfsfdawe : entity work.knlqmirtu
    port map (tawb => hmqj, xzef => bijidts, jgjzihrda => boc, gzgvmvewn => ywlusj);
  
  -- Single-driven assignments
  xk <= (1.1_0 ns, 16#85# ms);
end gmyghdkxrt;



-- Seed after: 5750297950365051088,17047277710231705797
