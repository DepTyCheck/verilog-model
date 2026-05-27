-- Seed: 16298061781074277283,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity nkwcnrl is
  port (hxdzbfii : out std_logic_vector(2 to 0); fglubd : out boolean; dlklnz : out std_logic; wlurrv : buffer std_logic_vector(2 to 1));
end nkwcnrl;



architecture upw of nkwcnrl is
  
begin
  
end upw;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (vrthbienu : out integer; iwtjny : linkage std_logic_vector(4 to 3); uasauz : out time_vector(1 to 1));
end t;



architecture e of t is
  
begin
  
end e;



entity dkjes is
  port (dsunouzbsz : buffer time; hzzuiywi : buffer integer; zujyiucu : buffer real);
end dkjes;

library ieee;
use ieee.std_logic_1164.all;

architecture ajtxt of dkjes is
  signal kh : boolean;
  signal o : std_logic_vector(2 to 0);
  signal xziev : std_logic_vector(2 to 1);
  signal iup : std_logic;
  signal hlsrhqegr : boolean;
  signal norcjcoyjm : std_logic_vector(2 to 1);
  signal qaucomai : time_vector(1 to 1);
  signal nywfiv : std_logic_vector(4 to 3);
  signal dcudcfibkz : integer;
begin
  mm : entity work.t
    port map (vrthbienu => dcudcfibkz, iwtjny => nywfiv, uasauz => qaucomai);
  ltszy : entity work.nkwcnrl
    port map (hxdzbfii => norcjcoyjm, fglubd => hlsrhqegr, dlklnz => iup, wlurrv => xziev);
  vswrypn : entity work.nkwcnrl
    port map (hxdzbfii => o, fglubd => kh, dlklnz => iup, wlurrv => norcjcoyjm);
end ajtxt;



-- Seed after: 5076144233414490827,13854332967471039201
