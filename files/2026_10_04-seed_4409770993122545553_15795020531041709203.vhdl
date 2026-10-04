-- Seed: 4409770993122545553,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity ihsyeyevtl is
  port (pwrsb : out std_logic; sbiw : out real);
end ihsyeyevtl;

architecture apx of ihsyeyevtl is
  
begin
  -- Single-driven assignments
  sbiw <= 2#0_0.0_1#;
  
  -- Multi-driven assignments
  pwrsb <= 'Z';
  pwrsb <= pwrsb;
end apx;



-- Seed after: 11984363474869794631,15795020531041709203
