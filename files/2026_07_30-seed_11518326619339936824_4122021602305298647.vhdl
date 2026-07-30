-- Seed: 11518326619339936824,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity ohztipgoyc is
  port (wglhbpqzd : buffer time; rgwsewqqb : linkage std_logic_vector(0 to 2); gsh : linkage std_logic_vector(3 to 4));
end ohztipgoyc;

architecture q of ohztipgoyc is
  
begin
  -- Single-driven assignments
  wglhbpqzd <= 4 sec;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity hvytpu is
  port (aapprs : inout std_logic; jphw : linkage std_logic_vector(3 downto 1));
end hvytpu;

library ieee;
use ieee.std_logic_1164.all;

architecture njfb of hvytpu is
  signal ulqehouvd : std_logic_vector(3 to 4);
  signal qnyvhkh : time;
  signal svfy : std_logic_vector(3 to 4);
  signal j : std_logic_vector(0 to 2);
  signal nbrhsgyol : time;
  signal isupoh : time;
  signal niiodm : std_logic_vector(3 to 4);
  signal tb : time;
begin
  kgfcxdkosy : entity work.ohztipgoyc
    port map (wglhbpqzd => tb, rgwsewqqb => jphw, gsh => niiodm);
  a : entity work.ohztipgoyc
    port map (wglhbpqzd => isupoh, rgwsewqqb => jphw, gsh => niiodm);
  umap : entity work.ohztipgoyc
    port map (wglhbpqzd => nbrhsgyol, rgwsewqqb => j, gsh => svfy);
  sgtz : entity work.ohztipgoyc
    port map (wglhbpqzd => qnyvhkh, rgwsewqqb => jphw, gsh => ulqehouvd);
  
  -- Multi-driven assignments
  aapprs <= '1';
  niiodm <= niiodm;
  svfy <= "ZZ";
  svfy <= niiodm;
end njfb;



-- Seed after: 16552698879200134697,4122021602305298647
