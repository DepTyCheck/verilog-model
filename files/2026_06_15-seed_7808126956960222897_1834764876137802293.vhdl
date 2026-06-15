-- Seed: 7808126956960222897,1834764876137802293

entity mnb is
  port (edkfowk : inout real; trasnpinr : out string(2 to 2));
end mnb;

architecture jxc of mnb is
  
begin
  -- Single-driven assignments
  edkfowk <= 2#0.0111#;
  trasnpinr <= "x";
end jxc;

library ieee;
use ieee.std_logic_1164.all;

entity va is
  port (rnbyfb : buffer std_logic);
end va;

architecture dfvhnrc of va is
  signal xaudecsqvg : string(2 to 2);
  signal vy : real;
  signal awwqhhrztg : string(2 to 2);
  signal tbdypepry : real;
  signal zkqjw : string(2 to 2);
  signal csqzh : real;
  signal jyxifbs : string(2 to 2);
  signal bsu : real;
begin
  kelsoey : entity work.mnb
    port map (edkfowk => bsu, trasnpinr => jyxifbs);
  zozj : entity work.mnb
    port map (edkfowk => csqzh, trasnpinr => zkqjw);
  awnrf : entity work.mnb
    port map (edkfowk => tbdypepry, trasnpinr => awwqhhrztg);
  bdksbp : entity work.mnb
    port map (edkfowk => vy, trasnpinr => xaudecsqvg);
  
  -- Multi-driven assignments
  rnbyfb <= 'X';
end dfvhnrc;



-- Seed after: 11955119172822660212,1834764876137802293
