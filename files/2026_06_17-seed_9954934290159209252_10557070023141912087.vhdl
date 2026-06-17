-- Seed: 9954934290159209252,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity nlm is
  port (yfp : inout bit_vector(3 to 3); enric : out integer; pdenycquwi : linkage std_logic_vector(0 downto 1));
end nlm;

architecture w of nlm is
  
begin
  -- Single-driven assignments
  enric <= 4_1_0_4_2;
  yfp <= (others => '0');
end w;

library ieee;
use ieee.std_logic_1164.all;

entity cynyr is
  port (h : buffer boolean; hkmhbgdfo : inout std_logic_vector(0 downto 4); tmgq : inout integer);
end cynyr;

library ieee;
use ieee.std_logic_1164.all;

architecture mkqcbvxzy of cynyr is
  signal rebwxqv : bit_vector(3 to 3);
  signal toxpove : std_logic_vector(0 downto 1);
  signal z : integer;
  signal aey : bit_vector(3 to 3);
  signal iqmhoebysr : integer;
  signal qrfznxrswx : bit_vector(3 to 3);
begin
  rtldpezane : entity work.nlm
    port map (yfp => qrfznxrswx, enric => iqmhoebysr, pdenycquwi => hkmhbgdfo);
  nupbjk : entity work.nlm
    port map (yfp => aey, enric => z, pdenycquwi => toxpove);
  amqbjstn : entity work.nlm
    port map (yfp => rebwxqv, enric => tmgq, pdenycquwi => hkmhbgdfo);
  
  -- Single-driven assignments
  h <= FALSE;
  
  -- Multi-driven assignments
  hkmhbgdfo <= "";
  hkmhbgdfo <= "";
end mkqcbvxzy;



-- Seed after: 4636976763517638690,10557070023141912087
