-- Seed: 12207892265640818349,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity oonzwy is
  port ( xmvrbgvwnf : out std_logic
  ; egftievn : inout std_logic_vector(1 downto 4)
  ; hsxiab : linkage real_vector(0 to 1)
  ; mxpg : linkage integer_vector(4 downto 0)
  );
end oonzwy;

architecture btjhwgnqei of oonzwy is
  
begin
  -- Multi-driven assignments
  egftievn <= egftievn;
end btjhwgnqei;

library ieee;
use ieee.std_logic_1164.all;

entity bdykj is
  port (ny : out std_logic; ll : inout time; nwmlxdktl : in std_logic; wcql : inout std_logic);
end bdykj;

library ieee;
use ieee.std_logic_1164.all;

architecture nawwngm of bdykj is
  signal qclzp : integer_vector(4 downto 0);
  signal tmcx : real_vector(0 to 1);
  signal adenadfuqb : std_logic;
  signal yplownm : integer_vector(4 downto 0);
  signal fmtatq : real_vector(0 to 1);
  signal dc : std_logic_vector(1 downto 4);
  signal sp : std_logic;
begin
  auyypbs : entity work.oonzwy
    port map (xmvrbgvwnf => sp, egftievn => dc, hsxiab => fmtatq, mxpg => yplownm);
  ienjbtd : entity work.oonzwy
    port map (xmvrbgvwnf => adenadfuqb, egftievn => dc, hsxiab => tmcx, mxpg => qclzp);
  
  -- Multi-driven assignments
  dc <= "";
  dc <= (others => '0');
  wcql <= 'L';
  wcql <= '1';
end nawwngm;

library ieee;
use ieee.std_logic_1164.all;

entity ue is
  port (upgowcokk : in integer; ptelxuq : out std_logic; hb : inout bit);
end ue;

library ieee;
use ieee.std_logic_1164.all;

architecture n of ue is
  signal kkhknch : integer_vector(4 downto 0);
  signal j : real_vector(0 to 1);
  signal cist : std_logic;
  signal tfsgofemg : time;
  signal sv : integer_vector(4 downto 0);
  signal acqdnutpw : real_vector(0 to 1);
  signal xol : std_logic_vector(1 downto 4);
  signal fkmjoam : std_logic;
begin
  m : entity work.oonzwy
    port map (xmvrbgvwnf => fkmjoam, egftievn => xol, hsxiab => acqdnutpw, mxpg => sv);
  lbmjcusb : entity work.bdykj
    port map (ny => ptelxuq, ll => tfsgofemg, nwmlxdktl => ptelxuq, wcql => cist);
  nadezza : entity work.oonzwy
    port map (xmvrbgvwnf => cist, egftievn => xol, hsxiab => j, mxpg => kkhknch);
  
  -- Single-driven assignments
  hb <= '0';
  
  -- Multi-driven assignments
  xol <= "";
  fkmjoam <= '-';
  xol <= xol;
end n;



-- Seed after: 16099102753518609133,3316342841050048249
