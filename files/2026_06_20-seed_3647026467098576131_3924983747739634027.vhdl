-- Seed: 3647026467098576131,3924983747739634027

entity mzqohvk is
  port (ypfh : linkage time; eytkk : out integer);
end mzqohvk;

architecture gfiy of mzqohvk is
  
begin
  -- Single-driven assignments
  eytkk <= 8#2#;
end gfiy;

entity wfxxwlj is
  port (svab : in real; tkr : out time_vector(1 to 1); bpn : buffer real_vector(2 to 1));
end wfxxwlj;

architecture nazb of wfxxwlj is
  
begin
  -- Single-driven assignments
  bpn <= (others => 0.0);
  tkr <= (others => 243 us);
end nazb;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (yh : inout integer; udgm : buffer integer; e : inout std_logic);
end d;

architecture acejkrbiht of d is
  signal bynh : integer;
  signal vyrm : time;
  signal yazjoirxof : time;
  signal jagddocoom : time;
begin
  grffsl : entity work.mzqohvk
    port map (ypfh => jagddocoom, eytkk => udgm);
  totjpqgr : entity work.mzqohvk
    port map (ypfh => yazjoirxof, eytkk => yh);
  h : entity work.mzqohvk
    port map (ypfh => vyrm, eytkk => bynh);
end acejkrbiht;



-- Seed after: 7416268122680699246,3924983747739634027
