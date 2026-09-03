-- Seed: 11447630327201637579,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (bdgalamyft : in std_logic_vector(0 downto 0); tklgrol : inout integer_vector(4 to 3));
end h;

architecture gmwpf of h is
  
begin
  -- Single-driven assignments
  tklgrol <= (others => 0);
end gmwpf;

library ieee;
use ieee.std_logic_1164.all;

entity ecw is
  port (inlvxmp : buffer real; vwp : out std_logic; srhgeazjht : linkage bit_vector(1 downto 1); avvoi : out severity_level);
end ecw;

library ieee;
use ieee.std_logic_1164.all;

architecture k of ecw is
  signal ywxxafq : integer_vector(4 to 3);
  signal spinbextya : std_logic_vector(0 downto 0);
  signal odzhquy : integer_vector(4 to 3);
  signal jnk : std_logic_vector(0 downto 0);
begin
  zyaymeirtv : entity work.h
    port map (bdgalamyft => jnk, tklgrol => odzhquy);
  yvqvrzg : entity work.h
    port map (bdgalamyft => spinbextya, tklgrol => ywxxafq);
end k;

library ieee;
use ieee.std_logic_1164.all;

entity qhw is
  port (xrt : buffer time; tybius : in std_logic; mkfuojyvyq : out std_logic_vector(0 to 3));
end qhw;

architecture lnakzwvq of qhw is
  
begin
  -- Single-driven assignments
  xrt <= 1 min;
end lnakzwvq;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (xiwhcsy : out integer; r : buffer std_logic; xhea : in time; pupn : linkage std_logic_vector(0 downto 2));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture t of p is
  signal ksclaeuno : integer_vector(4 to 3);
  signal kbxz : std_logic_vector(0 downto 0);
  signal mirg : integer_vector(4 to 3);
  signal okcx : std_logic_vector(0 downto 0);
  signal hvhk : std_logic_vector(0 to 3);
  signal w : std_logic;
  signal xnppfac : time;
  signal c : severity_level;
  signal isrk : bit_vector(1 downto 1);
  signal olicpiyhbj : real;
begin
  xv : entity work.ecw
    port map (inlvxmp => olicpiyhbj, vwp => r, srhgeazjht => isrk, avvoi => c);
  i : entity work.qhw
    port map (xrt => xnppfac, tybius => w, mkfuojyvyq => hvhk);
  sva : entity work.h
    port map (bdgalamyft => okcx, tklgrol => mirg);
  ruapi : entity work.h
    port map (bdgalamyft => kbxz, tklgrol => ksclaeuno);
  
  -- Single-driven assignments
  xiwhcsy <= 0442;
  
  -- Multi-driven assignments
  w <= 'U';
  r <= 'L';
  r <= 'Z';
end t;



-- Seed after: 10987527331156730837,11127274767545411571
