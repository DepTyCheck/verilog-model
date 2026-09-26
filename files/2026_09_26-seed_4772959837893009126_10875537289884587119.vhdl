-- Seed: 4772959837893009126,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity lgdmty is
  port (fj : buffer std_logic; uhxatfx : in std_logic; jumxr : inout std_logic; ys : in time);
end lgdmty;

architecture hzyzqjop of lgdmty is
  
begin
  
end hzyzqjop;

entity kymqrimwwr is
  port (ysfxphsu : out integer; yqrivdvgf : out time; jpvcbawq : linkage boolean);
end kymqrimwwr;

library ieee;
use ieee.std_logic_1164.all;

architecture duhqahawn of kymqrimwwr is
  signal xqjfe : time;
  signal kzqyfyssmm : std_logic;
  signal wxgcahg : time;
  signal tfqj : std_logic;
  signal zzgffnjer : std_logic;
begin
  h : entity work.lgdmty
    port map (fj => zzgffnjer, uhxatfx => zzgffnjer, jumxr => tfqj, ys => wxgcahg);
  wioicekh : entity work.lgdmty
    port map (fj => zzgffnjer, uhxatfx => zzgffnjer, jumxr => kzqyfyssmm, ys => xqjfe);
  
  -- Single-driven assignments
  yqrivdvgf <= 16#A_0_3_B_E# fs;
  xqjfe <= 8#4.0# fs;
  wxgcahg <= yqrivdvgf;
  ysfxphsu <= ysfxphsu;
  
  -- Multi-driven assignments
  zzgffnjer <= '-';
  zzgffnjer <= 'H';
  zzgffnjer <= '-';
end duhqahawn;

library ieee;
use ieee.std_logic_1164.all;

entity wcyojzwkbn is
  port (myzbrqhdv : linkage string(2 to 2); t : in std_logic_vector(3 to 2));
end wcyojzwkbn;

architecture ixqw of wcyojzwkbn is
  signal p : boolean;
  signal pf : time;
  signal hwyciaglr : integer;
begin
  kvnomozgyx : entity work.kymqrimwwr
    port map (ysfxphsu => hwyciaglr, yqrivdvgf => pf, jpvcbawq => p);
end ixqw;



-- Seed after: 16229731080837692454,10875537289884587119
