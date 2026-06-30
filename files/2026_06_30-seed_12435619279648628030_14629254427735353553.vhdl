-- Seed: 12435619279648628030,14629254427735353553

entity eu is
  port (o : in boolean_vector(4 to 2));
end eu;

architecture btzwr of eu is
  
begin
  
end btzwr;

entity sxj is
  port (mduiabme : linkage real; wodcczm : linkage integer_vector(2 to 3); jvj : in integer);
end sxj;

architecture ywazwx of sxj is
  signal tviyda : boolean_vector(4 to 2);
  signal y : boolean_vector(4 to 2);
begin
  lwxjbn : entity work.eu
    port map (o => y);
  ffp : entity work.eu
    port map (o => tviyda);
  
  -- Single-driven assignments
  y <= (others => TRUE);
end ywazwx;

library ieee;
use ieee.std_logic_1164.all;

entity gerpyindq is
  port (zmuruq : buffer real; bxe : buffer std_logic; jb : linkage real);
end gerpyindq;

architecture pakz of gerpyindq is
  
begin
  -- Single-driven assignments
  zmuruq <= 3_3.0;
  
  -- Multi-driven assignments
  bxe <= 'H';
  bxe <= 'L';
  bxe <= '0';
  bxe <= 'U';
end pakz;



-- Seed after: 4742455267186461417,14629254427735353553
