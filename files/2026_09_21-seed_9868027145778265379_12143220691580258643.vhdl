-- Seed: 9868027145778265379,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity kz is
  port (nh : in integer; vzdho : linkage std_logic_vector(2 downto 1); cbpfcbvqf : inout time);
end kz;

architecture dlzpfdnk of kz is
  
begin
  -- Single-driven assignments
  cbpfcbvqf <= 2#1.0111# ns;
end dlzpfdnk;

entity zenhwlhwqu is
  port (hlohmgba : inout boolean_vector(4 to 4));
end zenhwlhwqu;

architecture ufnuhpjnvx of zenhwlhwqu is
  
begin
  -- Single-driven assignments
  hlohmgba <= (others => TRUE);
end ufnuhpjnvx;

entity jolbwz is
  port (vjbhxxxti : out time);
end jolbwz;

library ieee;
use ieee.std_logic_1164.all;

architecture gniz of jolbwz is
  signal minmhwh : time;
  signal sssqhlp : integer;
  signal mdewv : boolean_vector(4 to 4);
  signal lc : time;
  signal sqfwfil : std_logic_vector(2 downto 1);
  signal b : std_logic_vector(2 downto 1);
  signal webdshv : integer;
begin
  ebirevvdz : entity work.kz
    port map (nh => webdshv, vzdho => b, cbpfcbvqf => vjbhxxxti);
  qbugr : entity work.kz
    port map (nh => webdshv, vzdho => sqfwfil, cbpfcbvqf => lc);
  kk : entity work.zenhwlhwqu
    port map (hlohmgba => mdewv);
  ddrhx : entity work.kz
    port map (nh => sssqhlp, vzdho => b, cbpfcbvqf => minmhwh);
  
  -- Single-driven assignments
  sssqhlp <= webdshv;
  
  -- Multi-driven assignments
  b <= "0L";
  b <= "1L";
  b <= ('X', '0');
  b <= ('0', '-');
end gniz;

library ieee;
use ieee.std_logic_1164.all;

entity yvojihmg is
  port (mqvmnqzmc : linkage boolean; wwq : in time; nzit : inout std_logic_vector(4 to 0); ei : in integer);
end yvojihmg;

architecture uvvioltj of yvojihmg is
  signal dhlljc : time;
begin
  nuvkljdpaj : entity work.jolbwz
    port map (vjbhxxxti => dhlljc);
  
  -- Multi-driven assignments
  nzit <= nzit;
  nzit <= "";
  nzit <= nzit;
end uvvioltj;



-- Seed after: 2188843048250020186,12143220691580258643
