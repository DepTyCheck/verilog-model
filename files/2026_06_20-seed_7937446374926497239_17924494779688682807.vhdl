-- Seed: 7937446374926497239,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity izfpfmb is
  port (bydi : in std_logic);
end izfpfmb;

architecture vjxozqf of izfpfmb is
  
begin
  
end vjxozqf;

entity rqebcg is
  port (nlp : out integer);
end rqebcg;

library ieee;
use ieee.std_logic_1164.all;

architecture runyzsib of rqebcg is
  signal lylwchovrc : std_logic;
  signal lfmvcz : std_logic;
  signal n : std_logic;
begin
  vmj : entity work.izfpfmb
    port map (bydi => n);
  edyz : entity work.izfpfmb
    port map (bydi => n);
  jp : entity work.izfpfmb
    port map (bydi => lfmvcz);
  yqsyjc : entity work.izfpfmb
    port map (bydi => lylwchovrc);
  
  -- Single-driven assignments
  nlp <= 2#1010#;
  
  -- Multi-driven assignments
  n <= 'U';
end runyzsib;

library ieee;
use ieee.std_logic_1164.all;

entity cfuzoqxn is
  port (swo : inout string(4 to 2); dvb : in std_logic; qabq : linkage severity_level; lelvgyi : inout time);
end cfuzoqxn;

architecture jv of cfuzoqxn is
  
begin
  jjkbcznk : entity work.izfpfmb
    port map (bydi => dvb);
  l : entity work.izfpfmb
    port map (bydi => dvb);
  oqpgvzs : entity work.izfpfmb
    port map (bydi => dvb);
  aqpddma : entity work.izfpfmb
    port map (bydi => dvb);
  
  -- Single-driven assignments
  swo <= (others => ' ');
  lelvgyi <= 16#4# ns;
end jv;



-- Seed after: 12490018788105472636,17924494779688682807
