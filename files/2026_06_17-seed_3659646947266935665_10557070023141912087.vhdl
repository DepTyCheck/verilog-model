-- Seed: 3659646947266935665,10557070023141912087

entity sfcesuwdby is
  port (ndwpcrnpd : out real_vector(4 downto 3));
end sfcesuwdby;

architecture chnp of sfcesuwdby is
  
begin
  -- Single-driven assignments
  ndwpcrnpd <= (2.0_3_4_4_0, 2#0_1_1_0_0.1#);
end chnp;

entity cpvirc is
  port (ikmbcyuogr : inout time);
end cpvirc;

architecture eysvkxha of cpvirc is
  signal fza : real_vector(4 downto 3);
begin
  zjqhdbvxdz : entity work.sfcesuwdby
    port map (ndwpcrnpd => fza);
  
  -- Single-driven assignments
  ikmbcyuogr <= 1 hr;
end eysvkxha;

library ieee;
use ieee.std_logic_1164.all;

entity wbpwvnz is
  port (ueuyg : linkage time; o : linkage real; egixaerqk : in std_logic_vector(2 downto 2));
end wbpwvnz;

architecture izb of wbpwvnz is
  signal bpshh : time;
  signal zdf : time;
  signal tgqv : time;
  signal lwziolkl : real_vector(4 downto 3);
begin
  tbbwqawh : entity work.sfcesuwdby
    port map (ndwpcrnpd => lwziolkl);
  tlqepombgh : entity work.cpvirc
    port map (ikmbcyuogr => tgqv);
  a : entity work.cpvirc
    port map (ikmbcyuogr => zdf);
  yyuxr : entity work.cpvirc
    port map (ikmbcyuogr => bpshh);
end izb;



-- Seed after: 16648268928481417754,10557070023141912087
