-- Seed: 11186098572984843156,13479070923501788437

entity ciq is
  port (kyowe : inout real; sy : buffer real; dkluhxln : inout time; jminhzmiqq : out real);
end ciq;

architecture fiaqhirz of ciq is
  
begin
  -- Single-driven assignments
  jminhzmiqq <= 16#5C0.4B#;
  sy <= 2#11001.0_1_1#;
  dkluhxln <= 2#1010.0_1_0_0# us;
end fiaqhirz;

entity esvf is
  port (jqfqy : linkage integer; qh : in boolean_vector(4 downto 3); qmriszngzy : inout time);
end esvf;

architecture jvcoydavx of esvf is
  signal qe : real;
  signal qkq : real;
  signal lgjfxzf : real;
begin
  vuhgzkgcj : entity work.ciq
    port map (kyowe => lgjfxzf, sy => qkq, dkluhxln => qmriszngzy, jminhzmiqq => qe);
end jvcoydavx;

library ieee;
use ieee.std_logic_1164.all;

entity mrkuibkn is
  port (wcjvoc : in time; nmk : buffer std_logic_vector(1 downto 3); ptiazkna : in real);
end mrkuibkn;

architecture wzmssola of mrkuibkn is
  signal m : real;
  signal qvxtcxn : time;
  signal wj : real;
  signal htapsjg : real;
  signal dbxo : real;
  signal waebefs : time;
  signal vkdalcpp : real;
  signal ymy : real;
  signal zwippslu : real;
  signal ogqhqx : time;
  signal repympkfz : real;
  signal sdhlg : real;
  signal f : real;
  signal ouna : time;
  signal hatepgk : real;
  signal vvaruaqpp : real;
begin
  til : entity work.ciq
    port map (kyowe => vvaruaqpp, sy => hatepgk, dkluhxln => ouna, jminhzmiqq => f);
  fy : entity work.ciq
    port map (kyowe => sdhlg, sy => repympkfz, dkluhxln => ogqhqx, jminhzmiqq => zwippslu);
  mahrxp : entity work.ciq
    port map (kyowe => ymy, sy => vkdalcpp, dkluhxln => waebefs, jminhzmiqq => dbxo);
  vfbvelb : entity work.ciq
    port map (kyowe => htapsjg, sy => wj, dkluhxln => qvxtcxn, jminhzmiqq => m);
end wzmssola;



-- Seed after: 6888608347335283233,13479070923501788437
