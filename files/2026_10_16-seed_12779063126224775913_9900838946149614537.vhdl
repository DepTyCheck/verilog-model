-- Seed: 12779063126224775913,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (msqhnr : buffer std_logic; zvhxims : out real);
end w;

architecture xy of w is
  
begin
  -- Single-driven assignments
  zvhxims <= 4333.2;
end xy;

entity tbaoze is
  port (fxazmtbbtg : buffer time; zrn : linkage time);
end tbaoze;

library ieee;
use ieee.std_logic_1164.all;

architecture ui of tbaoze is
  signal mfow : real;
  signal vqd : real;
  signal e : std_logic;
  signal pnjgc : real;
  signal xqmm : real;
  signal v : std_logic;
begin
  ggg : entity work.w
    port map (msqhnr => v, zvhxims => xqmm);
  hukbswzzs : entity work.w
    port map (msqhnr => v, zvhxims => pnjgc);
  klac : entity work.w
    port map (msqhnr => e, zvhxims => vqd);
  fjl : entity work.w
    port map (msqhnr => e, zvhxims => mfow);
  
  -- Single-driven assignments
  fxazmtbbtg <= fxazmtbbtg;
  
  -- Multi-driven assignments
  v <= 'X';
  e <= v;
end ui;



-- Seed after: 7000934252881893394,9900838946149614537
