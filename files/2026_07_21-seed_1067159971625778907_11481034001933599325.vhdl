-- Seed: 1067159971625778907,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity gckl is
  port (ljng : out std_logic; s : inout time);
end gckl;

architecture mrf of gckl is
  
begin
  -- Single-driven assignments
  s <= 16#67.D# ps;
  
  -- Multi-driven assignments
  ljng <= '-';
  ljng <= ljng;
  ljng <= ljng;
  ljng <= ljng;
end mrf;

entity uldizj is
  port (jetbxk : linkage real);
end uldizj;

architecture tal of uldizj is
  
begin
  
end tal;



-- Seed after: 3298438616921302468,11481034001933599325
