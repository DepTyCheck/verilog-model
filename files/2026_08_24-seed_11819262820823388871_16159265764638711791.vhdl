-- Seed: 11819262820823388871,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity cuxrhsrt is
  port (oei : in std_logic_vector(3 to 3));
end cuxrhsrt;

architecture agmsc of cuxrhsrt is
  
begin
  
end agmsc;

library ieee;
use ieee.std_logic_1164.all;

entity rif is
  port (jwbhw : buffer real; rgkribjf : buffer std_logic);
end rif;

library ieee;
use ieee.std_logic_1164.all;

architecture bf of rif is
  signal jwvh : std_logic_vector(3 to 3);
begin
  jsifkd : entity work.cuxrhsrt
    port map (oei => jwvh);
  
  -- Single-driven assignments
  jwbhw <= jwbhw;
end bf;

entity metelpbq is
  port (crnwric : out real; yvtvjkg : in real);
end metelpbq;

library ieee;
use ieee.std_logic_1164.all;

architecture cvnbrx of metelpbq is
  signal hrtt : std_logic;
  signal pfi : std_logic_vector(3 to 3);
  signal vnydlecr : std_logic_vector(3 to 3);
begin
  dseulcga : entity work.cuxrhsrt
    port map (oei => vnydlecr);
  l : entity work.cuxrhsrt
    port map (oei => pfi);
  einmt : entity work.rif
    port map (jwbhw => crnwric, rgkribjf => hrtt);
end cvnbrx;



-- Seed after: 10224417252361368766,16159265764638711791
