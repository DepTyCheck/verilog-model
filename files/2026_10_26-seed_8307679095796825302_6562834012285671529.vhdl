-- Seed: 8307679095796825302,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity vmxga is
  port (c : in integer; orluubiu : out std_logic; rgqohdcy : out time);
end vmxga;

architecture ysmbc of vmxga is
  
begin
  -- Single-driven assignments
  rgqohdcy <= 4 fs;
end ysmbc;

entity u is
  port (sgpopr : buffer boolean; kvyedpmoky : in boolean_vector(3 to 1));
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture vjyhitozn of u is
  signal aq : time;
  signal micx : std_logic;
  signal hzl : integer;
  signal rgp : time;
  signal hlunbseam : std_logic;
  signal wspfco : integer;
begin
  pb : entity work.vmxga
    port map (c => wspfco, orluubiu => hlunbseam, rgqohdcy => rgp);
  aaow : entity work.vmxga
    port map (c => hzl, orluubiu => micx, rgqohdcy => aq);
  
  -- Single-driven assignments
  wspfco <= wspfco;
  sgpopr <= FALSE;
  
  -- Multi-driven assignments
  micx <= hlunbseam;
  hlunbseam <= hlunbseam;
  hlunbseam <= 'Z';
end vjyhitozn;



-- Seed after: 18416162858511323353,6562834012285671529
