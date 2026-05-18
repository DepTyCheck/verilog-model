-- Seed: 11479613673211984349,5362988889992816031



entity ep is
  port (q : inout real);
end ep;



architecture xneb of ep is
  
begin
  
end xneb;

library ieee;
use ieee.std_logic_1164.all;

entity hwcblekbwm is
  port (q : inout integer; khmdcwvk : linkage std_logic);
end hwcblekbwm;



architecture dxyqfsfbt of hwcblekbwm is
  signal ofxrz : real;
  signal cjpfovs : real;
  signal ofixubqkwi : real;
  signal ggphqulfa : real;
begin
  uwgpxcll : entity work.ep
    port map (q => ggphqulfa);
  psmpc : entity work.ep
    port map (q => ofixubqkwi);
  thspfpbom : entity work.ep
    port map (q => cjpfovs);
  mjpgdvhnn : entity work.ep
    port map (q => ofxrz);
end dxyqfsfbt;



entity iim is
  port (jaxyvjxri : buffer time; xdea : buffer real);
end iim;

library ieee;
use ieee.std_logic_1164.all;

architecture gicninsyt of iim is
  signal pcu : real;
  signal zgsae : std_logic;
  signal alurcfrokx : integer;
begin
  yi : entity work.hwcblekbwm
    port map (q => alurcfrokx, khmdcwvk => zgsae);
  riyfmvffrq : entity work.ep
    port map (q => pcu);
  nvuymobmou : entity work.ep
    port map (q => xdea);
end gicninsyt;



entity vkfq is
  port (flnaqe : out real);
end vkfq;



architecture oz of vkfq is
  signal ho : real;
  signal xyusuwzb : time;
begin
  kzwuw : entity work.iim
    port map (jaxyvjxri => xyusuwzb, xdea => ho);
end oz;



-- Seed after: 7787015670230243487,5362988889992816031
