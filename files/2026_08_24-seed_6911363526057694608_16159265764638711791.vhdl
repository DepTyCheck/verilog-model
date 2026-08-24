-- Seed: 6911363526057694608,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity rwswyr is
  port (rmd : linkage std_logic; nqx : inout std_logic; m : out boolean_vector(2 to 4));
end rwswyr;

architecture x of rwswyr is
  
begin
  -- Single-driven assignments
  m <= (TRUE, TRUE, TRUE);
  
  -- Multi-driven assignments
  nqx <= 'U';
  nqx <= 'L';
end x;

entity h is
  port (xwcs : in time; xjwmm : inout integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture xxetjj of h is
  signal xlbtwcwph : boolean_vector(2 to 4);
  signal lyiwpeesax : std_logic;
  signal w : std_logic;
  signal wjlqghq : boolean_vector(2 to 4);
  signal gxqyos : std_logic;
  signal caf : boolean_vector(2 to 4);
  signal br : std_logic;
  signal rdcy : std_logic;
begin
  kfjdwwa : entity work.rwswyr
    port map (rmd => rdcy, nqx => br, m => caf);
  nsx : entity work.rwswyr
    port map (rmd => br, nqx => gxqyos, m => wjlqghq);
  pciedfpbvd : entity work.rwswyr
    port map (rmd => w, nqx => lyiwpeesax, m => xlbtwcwph);
  
  -- Single-driven assignments
  xjwmm <= xjwmm;
  
  -- Multi-driven assignments
  gxqyos <= 'Z';
  lyiwpeesax <= rdcy;
end xxetjj;



-- Seed after: 11662100533420663273,16159265764638711791
