-- Seed: 17045087510129150698,5805648483995786113

entity zaggzv is
  port (hdg : out real_vector(4 to 4));
end zaggzv;

architecture xdcqznwd of zaggzv is
  
begin
  -- Single-driven assignments
  hdg <= hdg;
end xdcqznwd;

library ieee;
use ieee.std_logic_1164.all;

entity chxfhi is
  port (eml : buffer std_logic_vector(4 downto 2));
end chxfhi;

architecture uwgghhd of chxfhi is
  signal rscxtl : real_vector(4 to 4);
  signal f : real_vector(4 to 4);
begin
  cktpsyjf : entity work.zaggzv
    port map (hdg => f);
  we : entity work.zaggzv
    port map (hdg => rscxtl);
  
  -- Multi-driven assignments
  eml <= eml;
  eml <= "U11";
  eml <= "1-Z";
end uwgghhd;



-- Seed after: 8884818928297874116,5805648483995786113
