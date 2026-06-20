-- Seed: 12702110815806060139,3924983747739634027

entity zjappgtzf is
  port (mjrvshw : out real);
end zjappgtzf;

architecture mvminbkfk of zjappgtzf is
  
begin
  
end mvminbkfk;

library ieee;
use ieee.std_logic_1164.all;

entity qqarvzqxrs is
  port (fn : in time; hmprrvyqk : out integer; t : inout std_logic_vector(0 downto 3));
end qqarvzqxrs;

architecture jiqfa of qqarvzqxrs is
  signal hhofhwlt : real;
  signal uxn : real;
  signal mihnaozla : real;
  signal ov : real;
begin
  wc : entity work.zjappgtzf
    port map (mjrvshw => ov);
  wuhjcoshj : entity work.zjappgtzf
    port map (mjrvshw => mihnaozla);
  gfpa : entity work.zjappgtzf
    port map (mjrvshw => uxn);
  qsabua : entity work.zjappgtzf
    port map (mjrvshw => hhofhwlt);
  
  -- Single-driven assignments
  hmprrvyqk <= 2_3;
  
  -- Multi-driven assignments
  t <= (others => '0');
  t <= (others => '0');
  t <= (others => '0');
end jiqfa;

entity ibnnlg is
  port (p : in integer);
end ibnnlg;

library ieee;
use ieee.std_logic_1164.all;

architecture csfwwg of ibnnlg is
  signal vhr : std_logic_vector(0 downto 3);
  signal rcsfc : integer;
  signal tpc : time;
begin
  wubbiczrf : entity work.qqarvzqxrs
    port map (fn => tpc, hmprrvyqk => rcsfc, t => vhr);
  
  -- Single-driven assignments
  tpc <= 8#0_7_3_1_5.3_2_7_0# us;
  
  -- Multi-driven assignments
  vhr <= "";
  vhr <= "";
  vhr <= (others => '0');
end csfwwg;



-- Seed after: 10264768867011426388,3924983747739634027
