-- Seed: 8990867064193896641,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity iookmyeqt is
  port (wfl : inout std_logic_vector(1 to 3));
end iookmyeqt;

architecture jdqajexxia of iookmyeqt is
  
begin
  
end jdqajexxia;

entity cejajuy is
  port (mttcwm : in integer);
end cejajuy;

library ieee;
use ieee.std_logic_1164.all;

architecture lhwpuy of cejajuy is
  signal ha : std_logic_vector(1 to 3);
begin
  nau : entity work.iookmyeqt
    port map (wfl => ha);
  jektdtvxg : entity work.iookmyeqt
    port map (wfl => ha);
  krz : entity work.iookmyeqt
    port map (wfl => ha);
  
  -- Multi-driven assignments
  ha <= ha;
  ha <= ('U', 'L', '0');
  ha <= "0XX";
end lhwpuy;



-- Seed after: 5066000655145743282,10975587770517081373
