-- Seed: 4027894079659532465,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity nuyhrqvb is
  port (wqndq : in std_logic_vector(1 to 2); yte : in real; fued : in std_logic);
end nuyhrqvb;

architecture mhtb of nuyhrqvb is
  
begin
  
end mhtb;

entity khiwhx is
  port (fhaqux : buffer time; kburwiie : in time; y : out time);
end khiwhx;

library ieee;
use ieee.std_logic_1164.all;

architecture k of khiwhx is
  signal szroeny : std_logic;
  signal erjmt : real;
  signal kf : std_logic;
  signal r : real;
  signal pduckoovnv : std_logic_vector(1 to 2);
begin
  j : entity work.nuyhrqvb
    port map (wqndq => pduckoovnv, yte => r, fued => kf);
  gperiku : entity work.nuyhrqvb
    port map (wqndq => pduckoovnv, yte => erjmt, fued => szroeny);
  
  -- Single-driven assignments
  y <= 3 us;
  r <= 042.4_0_3_2_1;
  erjmt <= 8#07.44#;
  fhaqux <= 3.4 ms;
  
  -- Multi-driven assignments
  kf <= 'Z';
end k;

library ieee;
use ieee.std_logic_1164.all;

entity fssuiz is
  port (npvrimqkpn : out integer_vector(1 downto 1); sg : linkage std_logic);
end fssuiz;

architecture hpu of fssuiz is
  signal qqjbyej : time;
  signal ecbrwamh : time;
  signal kvwz : time;
  signal iggppyfhfc : time;
  signal xhot : time;
  signal iuqn : time;
  signal ecgynb : time;
  signal r : time;
begin
  ctfktrhych : entity work.khiwhx
    port map (fhaqux => r, kburwiie => ecgynb, y => iuqn);
  igg : entity work.khiwhx
    port map (fhaqux => xhot, kburwiie => iggppyfhfc, y => kvwz);
  zcwvpf : entity work.khiwhx
    port map (fhaqux => ecgynb, kburwiie => ecbrwamh, y => qqjbyej);
  u : entity work.khiwhx
    port map (fhaqux => iggppyfhfc, kburwiie => kvwz, y => ecbrwamh);
end hpu;



-- Seed after: 2848260107072853543,14652815260262078753
