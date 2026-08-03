-- Seed: 213406750083936147,12359743974512393525

entity gsric is
  port (rwnavvc : out real; twzy : inout integer);
end gsric;

architecture hyuqoczxa of gsric is
  
begin
  -- Single-driven assignments
  rwnavvc <= 2#00110.1_1_0_0#;
end hyuqoczxa;

library ieee;
use ieee.std_logic_1164.all;

entity tokv is
  port (daqpaqmx : inout std_logic_vector(2 downto 0); xhc : inout integer; oeowxrm : buffer boolean; qmw : in std_logic_vector(2 to 4));
end tokv;

architecture l of tokv is
  signal fdpo : integer;
  signal m : real;
  signal huupiom : real;
  signal tnylfjlvya : integer;
  signal um : real;
begin
  gpzwgsl : entity work.gsric
    port map (rwnavvc => um, twzy => tnylfjlvya);
  egcaagnl : entity work.gsric
    port map (rwnavvc => huupiom, twzy => xhc);
  yssognc : entity work.gsric
    port map (rwnavvc => m, twzy => fdpo);
  
  -- Single-driven assignments
  oeowxrm <= oeowxrm;
  
  -- Multi-driven assignments
  daqpaqmx <= "XUL";
  daqpaqmx <= ('0', '0', 'X');
end l;

entity lfvwowduha is
  port (cruv : inout integer; lzlpguuqv : inout real);
end lfvwowduha;

architecture aj of lfvwowduha is
  signal zxgf : real;
  signal wpzmt : integer;
  signal i : integer;
  signal gvsdva : real;
begin
  wkjh : entity work.gsric
    port map (rwnavvc => gvsdva, twzy => i);
  h : entity work.gsric
    port map (rwnavvc => lzlpguuqv, twzy => wpzmt);
  rerfvoagtq : entity work.gsric
    port map (rwnavvc => zxgf, twzy => cruv);
end aj;

entity qfrjqns is
  port (e : buffer time);
end qfrjqns;

architecture uhfwrgqna of qfrjqns is
  signal zuybeln : real;
  signal fikvfh : integer;
  signal hj : real;
  signal kg : integer;
  signal zwravsquix : real;
  signal dyvqcjf : integer;
begin
  twd : entity work.lfvwowduha
    port map (cruv => dyvqcjf, lzlpguuqv => zwravsquix);
  hxdzx : entity work.lfvwowduha
    port map (cruv => kg, lzlpguuqv => hj);
  jg : entity work.lfvwowduha
    port map (cruv => fikvfh, lzlpguuqv => zuybeln);
  
  -- Single-driven assignments
  e <= 8#1_0# ms;
end uhfwrgqna;



-- Seed after: 8659028508602456445,12359743974512393525
