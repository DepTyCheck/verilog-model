-- Seed: 5050911608631243195,10240345754018108067

library ieee;
use ieee.std_logic_1164.all;

entity kjwndbl is
  port ( mbeweq : in boolean
  ; ervubefqh : linkage boolean_vector(4 to 4)
  ; bb : inout std_logic_vector(3 to 4)
  ; ynq : linkage std_logic_vector(3 downto 1)
  );
end kjwndbl;



architecture rjgluklp of kjwndbl is
  
begin
  
end rjgluklp;

library ieee;
use ieee.std_logic_1164.all;

entity pfmbulbnaf is
  port (mjorudbaj : in std_logic; quui : in integer; mmzlkj : buffer boolean);
end pfmbulbnaf;

library ieee;
use ieee.std_logic_1164.all;

architecture qytpwcl of pfmbulbnaf is
  signal hvaay : boolean_vector(4 to 4);
  signal tpcggsiqix : std_logic_vector(3 to 4);
  signal qtqrqybkdl : std_logic_vector(3 downto 1);
  signal cjsdtod : std_logic_vector(3 to 4);
  signal trcmsgf : boolean_vector(4 to 4);
begin
  wmyh : entity work.kjwndbl
    port map (mbeweq => mmzlkj, ervubefqh => trcmsgf, bb => cjsdtod, ynq => qtqrqybkdl);
  outhbmbvw : entity work.kjwndbl
    port map (mbeweq => mmzlkj, ervubefqh => trcmsgf, bb => tpcggsiqix, ynq => qtqrqybkdl);
  h : entity work.kjwndbl
    port map (mbeweq => mmzlkj, ervubefqh => hvaay, bb => tpcggsiqix, ynq => qtqrqybkdl);
end qytpwcl;

library ieee;
use ieee.std_logic_1164.all;

entity hkkojtthq is
  port (ptymyb : inout std_logic; kjoedw : out real; ls : in boolean);
end hkkojtthq;

library ieee;
use ieee.std_logic_1164.all;

architecture hkx of hkkojtthq is
  signal jysmpbsw : std_logic_vector(3 to 4);
  signal ltxowjmskm : boolean_vector(4 to 4);
  signal hufauza : std_logic_vector(3 downto 1);
  signal yspez : std_logic_vector(3 to 4);
  signal rlnuzqptai : boolean_vector(4 to 4);
  signal ezjlqxq : boolean;
begin
  vnnnj : entity work.kjwndbl
    port map (mbeweq => ezjlqxq, ervubefqh => rlnuzqptai, bb => yspez, ynq => hufauza);
  kir : entity work.kjwndbl
    port map (mbeweq => ls, ervubefqh => ltxowjmskm, bb => jysmpbsw, ynq => hufauza);
end hkx;



-- Seed after: 10074627499072081572,10240345754018108067
