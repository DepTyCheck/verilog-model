-- Seed: 16797244478638936321,13501862637168280927

entity mzhnlrwykq is
  port (m : in time; pfgbehqb : inout string(4 to 1));
end mzhnlrwykq;

architecture kn of mzhnlrwykq is
  
begin
  -- Single-driven assignments
  pfgbehqb <= pfgbehqb;
end kn;

library ieee;
use ieee.std_logic_1164.all;

entity acfqbuod is
  port (odeefc : out time; ylxlwukrlt : out std_logic_vector(1 downto 2));
end acfqbuod;

architecture fkyzo of acfqbuod is
  signal kz : string(4 to 1);
  signal so : string(4 to 1);
  signal krib : time;
  signal fkws : string(4 to 1);
begin
  mtlx : entity work.mzhnlrwykq
    port map (m => odeefc, pfgbehqb => fkws);
  xk : entity work.mzhnlrwykq
    port map (m => krib, pfgbehqb => so);
  lezguaerx : entity work.mzhnlrwykq
    port map (m => odeefc, pfgbehqb => kz);
  
  -- Single-driven assignments
  odeefc <= odeefc;
  krib <= 1 min;
  
  -- Multi-driven assignments
  ylxlwukrlt <= "";
end fkyzo;



-- Seed after: 18415944038405693984,13501862637168280927
