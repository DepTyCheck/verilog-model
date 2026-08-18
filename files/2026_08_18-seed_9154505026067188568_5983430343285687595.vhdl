-- Seed: 9154505026067188568,5983430343285687595

entity javilc is
  port (rzfjeayw : out integer_vector(2 to 0); lubz : out time);
end javilc;

architecture lvgt of javilc is
  
begin
  -- Single-driven assignments
  lubz <= 0_3_3_4_4.41 ms;
  rzfjeayw <= (others => 0);
end lvgt;

entity lpzvobg is
  port (nqcmmywe : linkage real; ermddpko : in character);
end lpzvobg;

architecture oglyd of lpzvobg is
  signal mouid : time;
  signal ewbted : integer_vector(2 to 0);
  signal xgzhwbm : time;
  signal lk : integer_vector(2 to 0);
begin
  hfut : entity work.javilc
    port map (rzfjeayw => lk, lubz => xgzhwbm);
  wju : entity work.javilc
    port map (rzfjeayw => ewbted, lubz => mouid);
end oglyd;

entity bgtesl is
  port (ruygcwissh : inout integer);
end bgtesl;

architecture t of bgtesl is
  signal pya : time;
  signal lfpf : integer_vector(2 to 0);
begin
  vksxfzmv : entity work.javilc
    port map (rzfjeayw => lfpf, lubz => pya);
  
  -- Single-driven assignments
  ruygcwissh <= 4_0_0_3;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity bwxxj is
  port (oxkrxjbm : in std_logic_vector(4 downto 4); z : inout real);
end bwxxj;

architecture zxfhtwraql of bwxxj is
  signal umtmbgykgp : time;
  signal nfpsgm : integer_vector(2 to 0);
  signal beymium : time;
  signal ltcgemt : integer_vector(2 to 0);
  signal ejc : integer;
begin
  tadlffexzz : entity work.bgtesl
    port map (ruygcwissh => ejc);
  nqlgduqyo : entity work.javilc
    port map (rzfjeayw => ltcgemt, lubz => beymium);
  jjzr : entity work.javilc
    port map (rzfjeayw => nfpsgm, lubz => umtmbgykgp);
  
  -- Single-driven assignments
  z <= 2#0_1.0_0_0#;
end zxfhtwraql;



-- Seed after: 10332226972091837268,5983430343285687595
