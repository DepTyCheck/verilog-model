-- Seed: 10897077603973149184,12431929723978769775



entity uxsstlvnt is
  port (xhcujdvmzb : out integer; t : in real);
end uxsstlvnt;



architecture a of uxsstlvnt is
  
begin
  
end a;



entity vxgvwfgoso is
  port (fqvrizbnzz : out time; taanybjnpc : in time; ypjoncil : in integer);
end vxgvwfgoso;



architecture mddd of vxgvwfgoso is
  signal ymjdy : real;
  signal voyveccgn : integer;
begin
  mr : entity work.uxsstlvnt
    port map (xhcujdvmzb => voyveccgn, t => ymjdy);
end mddd;



entity bjrgvcnn is
  port (r : buffer real);
end bjrgvcnn;



architecture yisd of bjrgvcnn is
  signal tbu : integer;
  signal oewcaipjae : integer;
  signal pxzkziyzn : time;
  signal jks : time;
  signal kkzcw : integer;
  signal qcmkaivnvm : integer;
begin
  yymrh : entity work.uxsstlvnt
    port map (xhcujdvmzb => qcmkaivnvm, t => r);
  bzdyeuvuon : entity work.uxsstlvnt
    port map (xhcujdvmzb => kkzcw, t => r);
  rsep : entity work.vxgvwfgoso
    port map (fqvrizbnzz => jks, taanybjnpc => pxzkziyzn, ypjoncil => oewcaipjae);
  nvyv : entity work.vxgvwfgoso
    port map (fqvrizbnzz => pxzkziyzn, taanybjnpc => jks, ypjoncil => tbu);
end yisd;

library ieee;
use ieee.std_logic_1164.all;

entity volavnnlaa is
  port (rbyryodvrn : buffer std_logic; pdvuxksa : out integer; n : inout bit_vector(4 downto 1));
end volavnnlaa;



architecture fqkqpehpyq of volavnnlaa is
  signal oebw : time;
begin
  ugkgpehmcv : entity work.vxgvwfgoso
    port map (fqvrizbnzz => oebw, taanybjnpc => oebw, ypjoncil => pdvuxksa);
end fqkqpehpyq;



-- Seed after: 12150800500034769198,12431929723978769775
