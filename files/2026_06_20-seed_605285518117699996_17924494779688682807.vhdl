-- Seed: 605285518117699996,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity apbpigyuh is
  port (sxbxmban : in character; ip : out std_logic_vector(3 to 2));
end apbpigyuh;

architecture ktja of apbpigyuh is
  
begin
  -- Multi-driven assignments
  ip <= (others => '0');
  ip <= "";
end ktja;

library ieee;
use ieee.std_logic_1164.all;

entity ycyjkfpxp is
  port (hf : in severity_level; shqwagrvi : linkage real; boooo : out std_logic_vector(4 to 3));
end ycyjkfpxp;

architecture j of ycyjkfpxp is
  signal v : character;
begin
  mdjx : entity work.apbpigyuh
    port map (sxbxmban => v, ip => boooo);
  
  -- Single-driven assignments
  v <= 'j';
end j;

entity zzcfs is
  port (ge : in severity_level; amxb : buffer real_vector(0 to 1));
end zzcfs;

library ieee;
use ieee.std_logic_1164.all;

architecture dlvirt of zzcfs is
  signal wiydkcse : std_logic_vector(4 to 3);
  signal zqciqyck : real;
begin
  vdkooz : entity work.ycyjkfpxp
    port map (hf => ge, shqwagrvi => zqciqyck, boooo => wiydkcse);
  
  -- Single-driven assignments
  amxb <= (2#0.0#, 0_4_0.2);
  
  -- Multi-driven assignments
  wiydkcse <= "";
  wiydkcse <= (others => '0');
  wiydkcse <= "";
  wiydkcse <= (others => '0');
end dlvirt;



-- Seed after: 16075860383883463129,17924494779688682807
