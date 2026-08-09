-- Seed: 15226527452868036828,10871023049702252113

entity vwmgzykrhd is
  port (kcnqyuaxn : out real; oknfqf : out integer; u : out integer_vector(4 downto 0));
end vwmgzykrhd;

architecture zadgwiysj of vwmgzykrhd is
  
begin
  -- Single-driven assignments
  kcnqyuaxn <= 8#1.1_3#;
  oknfqf <= oknfqf;
end zadgwiysj;

library ieee;
use ieee.std_logic_1164.all;

entity hf is
  port (tvygzkzz : inout integer; giy : in integer; dtckhvgh : buffer std_logic_vector(1 to 4));
end hf;

architecture zbvf of hf is
  
begin
  -- Single-driven assignments
  tvygzkzz <= giy;
  
  -- Multi-driven assignments
  dtckhvgh <= dtckhvgh;
  dtckhvgh <= dtckhvgh;
  dtckhvgh <= ('0', '1', 'L', 'L');
  dtckhvgh <= dtckhvgh;
end zbvf;

entity jmwjnr is
  port (avpccym : buffer boolean_vector(3 downto 4); dqtaria : in integer);
end jmwjnr;

library ieee;
use ieee.std_logic_1164.all;

architecture ntwis of jmwjnr is
  signal gunxjjbwnk : std_logic_vector(1 to 4);
  signal wrn : integer;
  signal wpwtbh : integer_vector(4 downto 0);
  signal cscxbtm : real;
  signal h : std_logic_vector(1 to 4);
  signal cxtjftpy : integer;
  signal kcba : integer;
begin
  bszvcedego : entity work.hf
    port map (tvygzkzz => kcba, giy => cxtjftpy, dtckhvgh => h);
  huplnywqnl : entity work.vwmgzykrhd
    port map (kcnqyuaxn => cscxbtm, oknfqf => cxtjftpy, u => wpwtbh);
  gohbc : entity work.hf
    port map (tvygzkzz => wrn, giy => wrn, dtckhvgh => gunxjjbwnk);
  
  -- Single-driven assignments
  avpccym <= avpccym;
end ntwis;



-- Seed after: 11727458632549566608,10871023049702252113
