-- Seed: 11866294898820968620,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity amqycwa is
  port (dur : out std_logic_vector(4 to 1); fuc : buffer std_logic_vector(3 downto 4));
end amqycwa;

architecture gu of amqycwa is
  
begin
  -- Multi-driven assignments
  fuc <= (others => '0');
  dur <= fuc;
  fuc <= "";
  fuc <= "";
end gu;

library ieee;
use ieee.std_logic_1164.all;

entity rrxfsitw is
  port (xwgthczf : out real; egjgjwkn : linkage std_logic_vector(3 to 1); wmmvav : linkage time; tvstth : in character);
end rrxfsitw;

library ieee;
use ieee.std_logic_1164.all;

architecture olmfrpfr of rrxfsitw is
  signal g : std_logic_vector(3 downto 4);
  signal wduefncjn : std_logic_vector(4 to 1);
  signal fxkbynik : std_logic_vector(3 downto 4);
  signal ibmrvhv : std_logic_vector(4 to 1);
begin
  rhx : entity work.amqycwa
    port map (dur => ibmrvhv, fuc => fxkbynik);
  tspmfoez : entity work.amqycwa
    port map (dur => wduefncjn, fuc => g);
  wzfhfzv : entity work.amqycwa
    port map (dur => ibmrvhv, fuc => g);
  
  -- Single-driven assignments
  xwgthczf <= 16#5_C_1_E.3#;
  
  -- Multi-driven assignments
  g <= (others => '0');
  ibmrvhv <= "";
  ibmrvhv <= "";
end olmfrpfr;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (cmikyxasoo : out integer; lgf : inout std_logic);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture sreek of p is
  signal yixd : character;
  signal n : time;
  signal jcvsxviq : real;
  signal mn : character;
  signal uhhtnwrhs : time;
  signal pjafpu : std_logic_vector(3 to 1);
  signal h : real;
begin
  fq : entity work.rrxfsitw
    port map (xwgthczf => h, egjgjwkn => pjafpu, wmmvav => uhhtnwrhs, tvstth => mn);
  fxrmhud : entity work.rrxfsitw
    port map (xwgthczf => jcvsxviq, egjgjwkn => pjafpu, wmmvav => n, tvstth => yixd);
  
  -- Single-driven assignments
  cmikyxasoo <= 2#1#;
  yixd <= mn;
  mn <= 'h';
  
  -- Multi-driven assignments
  lgf <= lgf;
  pjafpu <= pjafpu;
end sreek;



-- Seed after: 15554877235633334970,2287986438313874379
