-- Seed: 10297209193469918615,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity gdmeri is
  port (yaru : in time; iiryrj : inout boolean; yqawalmi : inout std_logic_vector(1 to 4); nkxawh : buffer time);
end gdmeri;

architecture kzipz of gdmeri is
  
begin
  -- Single-driven assignments
  nkxawh <= 2#1_0_0_1.001# us;
  
  -- Multi-driven assignments
  yqawalmi <= ('1', 'Z', 'L', '-');
  yqawalmi <= yqawalmi;
end kzipz;

entity sepr is
  port (sq : out bit; i : buffer integer);
end sepr;

library ieee;
use ieee.std_logic_1164.all;

architecture rce of sepr is
  signal ulhte : std_logic_vector(1 to 4);
  signal g : boolean;
  signal ioh : time;
  signal qf : std_logic_vector(1 to 4);
  signal eezfk : boolean;
  signal cxvrmu : time;
  signal djy : std_logic_vector(1 to 4);
  signal ivvppllmq : boolean;
  signal nhjxjyvu : time;
  signal ybxynhfd : time;
  signal pxorwskk : std_logic_vector(1 to 4);
  signal md : boolean;
  signal fsepuhc : time;
begin
  n : entity work.gdmeri
    port map (yaru => fsepuhc, iiryrj => md, yqawalmi => pxorwskk, nkxawh => ybxynhfd);
  rua : entity work.gdmeri
    port map (yaru => nhjxjyvu, iiryrj => ivvppllmq, yqawalmi => djy, nkxawh => fsepuhc);
  fw : entity work.gdmeri
    port map (yaru => cxvrmu, iiryrj => eezfk, yqawalmi => qf, nkxawh => nhjxjyvu);
  zi : entity work.gdmeri
    port map (yaru => ioh, iiryrj => g, yqawalmi => ulhte, nkxawh => cxvrmu);
  
  -- Multi-driven assignments
  djy <= pxorwskk;
  djy <= "0-HX";
  djy <= "1HL0";
  pxorwskk <= pxorwskk;
end rce;

library ieee;
use ieee.std_logic_1164.all;

entity ihnmlid is
  port (diozyfokf : out std_logic_vector(2 to 1));
end ihnmlid;

library ieee;
use ieee.std_logic_1164.all;

architecture ve of ihnmlid is
  signal yycllgnn : time;
  signal jsxz : std_logic_vector(1 to 4);
  signal nmvytavej : boolean;
  signal tlegcnvgmy : time;
  signal laxolqifin : integer;
  signal jm : bit;
begin
  mlcrecvhti : entity work.sepr
    port map (sq => jm, i => laxolqifin);
  lzwuv : entity work.gdmeri
    port map (yaru => tlegcnvgmy, iiryrj => nmvytavej, yqawalmi => jsxz, nkxawh => yycllgnn);
  
  -- Multi-driven assignments
  jsxz <= ('-', '-', 'L', 'L');
end ve;



-- Seed after: 2293083129203621053,14141408946471626091
