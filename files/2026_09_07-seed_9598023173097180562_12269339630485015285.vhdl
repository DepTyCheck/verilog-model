-- Seed: 9598023173097180562,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity iwmir is
  port (vksm : out std_logic_vector(4 to 0); huwqsbmn : in std_logic; vcyoq : inout std_logic_vector(3 to 3); gaqdrknypa : buffer std_logic);
end iwmir;

architecture xrxt of iwmir is
  
begin
  -- Multi-driven assignments
  gaqdrknypa <= 'X';
  gaqdrknypa <= gaqdrknypa;
end xrxt;

library ieee;
use ieee.std_logic_1164.all;

entity dekzexqxb is
  port (wuyzdg : in std_logic; zwjxxhcan : in boolean_vector(0 downto 1));
end dekzexqxb;

library ieee;
use ieee.std_logic_1164.all;

architecture oenitnq of dekzexqxb is
  signal fuwpajmy : std_logic;
  signal y : std_logic_vector(3 to 3);
  signal zimphn : std_logic;
  signal cnmmdqxjc : std_logic_vector(4 to 0);
  signal cqodqvmxoz : std_logic;
  signal phxc : std_logic_vector(3 to 3);
  signal ykmsvcxt : std_logic;
  signal b : std_logic_vector(4 to 0);
begin
  nuoyjzc : entity work.iwmir
    port map (vksm => b, huwqsbmn => ykmsvcxt, vcyoq => phxc, gaqdrknypa => cqodqvmxoz);
  rivurjdkcv : entity work.iwmir
    port map (vksm => cnmmdqxjc, huwqsbmn => zimphn, vcyoq => y, gaqdrknypa => fuwpajmy);
  
  -- Multi-driven assignments
  y <= "W";
  cnmmdqxjc <= b;
  b <= (others => '0');
  y <= (others => '-');
end oenitnq;



-- Seed after: 11437891544770202334,12269339630485015285
