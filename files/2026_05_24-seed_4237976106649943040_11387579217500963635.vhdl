-- Seed: 4237976106649943040,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity mfte is
  port (osyjyyxc : buffer time_vector(4 downto 0); xxkssez : buffer std_logic; zn : inout std_logic);
end mfte;



architecture emkxucf of mfte is
  
begin
  
end emkxucf;



entity mpmejy is
  port (rkvmf : in real);
end mpmejy;



architecture bznzhg of mpmejy is
  
begin
  
end bznzhg;



entity xmyfgtve is
  port (rvyweiiqp : in real);
end xmyfgtve;

library ieee;
use ieee.std_logic_1164.all;

architecture q of xmyfgtve is
  signal sklde : std_logic;
  signal kueh : time_vector(4 downto 0);
  signal fgmfx : real;
  signal aiecjglu : std_logic;
  signal etpxw : std_logic;
  signal nwnopasnkj : time_vector(4 downto 0);
  signal eg : std_logic;
  signal ggjfxxtt : time_vector(4 downto 0);
begin
  vtaaunmvfm : entity work.mfte
    port map (osyjyyxc => ggjfxxtt, xxkssez => eg, zn => eg);
  xydcqddg : entity work.mfte
    port map (osyjyyxc => nwnopasnkj, xxkssez => etpxw, zn => aiecjglu);
  fh : entity work.mpmejy
    port map (rkvmf => fgmfx);
  vnpwexfa : entity work.mfte
    port map (osyjyyxc => kueh, xxkssez => sklde, zn => eg);
end q;



-- Seed after: 6847942996741153950,11387579217500963635
