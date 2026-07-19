-- Seed: 913092001678167265,5511103086789671269

entity qklffr is
  port (m : in integer; tn : buffer bit_vector(4 to 2); jqdblsg : in time);
end qklffr;

architecture cmjn of qklffr is
  
begin
  -- Single-driven assignments
  tn <= (others => '0');
end cmjn;

library ieee;
use ieee.std_logic_1164.all;

entity bcqfgh is
  port (zieslnds : in std_logic; wo : in integer);
end bcqfgh;

architecture iiwbiaxe of bcqfgh is
  signal wnvtearvb : bit_vector(4 to 2);
  signal bszfxgfdrq : integer;
  signal ia : bit_vector(4 to 2);
  signal vapp : bit_vector(4 to 2);
  signal trgoktkk : time;
  signal r : bit_vector(4 to 2);
begin
  amjonwnuge : entity work.qklffr
    port map (m => wo, tn => r, jqdblsg => trgoktkk);
  gtxvhlvjfz : entity work.qklffr
    port map (m => wo, tn => vapp, jqdblsg => trgoktkk);
  phona : entity work.qklffr
    port map (m => wo, tn => ia, jqdblsg => trgoktkk);
  f : entity work.qklffr
    port map (m => bszfxgfdrq, tn => wnvtearvb, jqdblsg => trgoktkk);
  
  -- Single-driven assignments
  bszfxgfdrq <= 2#1_0_1_0_1#;
  trgoktkk <= trgoktkk;
end iiwbiaxe;



-- Seed after: 4904291375928194515,5511103086789671269
