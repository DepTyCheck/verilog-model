-- Seed: 8030677410309266759,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity mpcsm is
  port (yboolb : linkage std_logic_vector(2 downto 2); byrwwjcef : linkage time; ptxnwv : in time; ur : linkage real);
end mpcsm;

architecture mtmqdh of mpcsm is
  
begin
  
end mtmqdh;

library ieee;
use ieee.std_logic_1164.all;

entity yjrxus is
  port (mjebhn : buffer std_logic_vector(0 to 4); jmaftqk : linkage time);
end yjrxus;

library ieee;
use ieee.std_logic_1164.all;

architecture kchw of yjrxus is
  signal pi : real;
  signal tfuhvmdpu : time;
  signal brzb : std_logic_vector(2 downto 2);
  signal oxzes : real;
  signal ihgwspsj : real;
  signal n : real;
  signal t : time;
  signal nce : time;
  signal z : std_logic_vector(2 downto 2);
begin
  fcjqdhgsvq : entity work.mpcsm
    port map (yboolb => z, byrwwjcef => nce, ptxnwv => t, ur => n);
  mhlq : entity work.mpcsm
    port map (yboolb => z, byrwwjcef => jmaftqk, ptxnwv => nce, ur => ihgwspsj);
  xsdjeo : entity work.mpcsm
    port map (yboolb => z, byrwwjcef => t, ptxnwv => t, ur => oxzes);
  uti : entity work.mpcsm
    port map (yboolb => brzb, byrwwjcef => tfuhvmdpu, ptxnwv => nce, ur => pi);
  
  -- Multi-driven assignments
  mjebhn <= "LUX-X";
  mjebhn <= mjebhn;
  brzb <= (others => '0');
end kchw;

entity htrdnpi is
  port (rykxyxfjk : out integer; nmoah : linkage integer);
end htrdnpi;

library ieee;
use ieee.std_logic_1164.all;

architecture onjx of htrdnpi is
  signal a : time;
  signal ebasacl : std_logic_vector(0 to 4);
  signal rlzampwl : real;
  signal uztqbzo : time;
  signal khzvtf : time;
  signal uiyhmirsl : std_logic_vector(2 downto 2);
  signal mlbkiynzc : real;
  signal vjkhevlwu : time;
  signal wfqpyjl : std_logic_vector(2 downto 2);
  signal nxlyevpsy : real;
  signal yzckuxb : time;
  signal wbpayuwt : std_logic_vector(2 downto 2);
begin
  vuzglpxef : entity work.mpcsm
    port map (yboolb => wbpayuwt, byrwwjcef => yzckuxb, ptxnwv => yzckuxb, ur => nxlyevpsy);
  wretrb : entity work.mpcsm
    port map (yboolb => wfqpyjl, byrwwjcef => vjkhevlwu, ptxnwv => vjkhevlwu, ur => mlbkiynzc);
  akhnm : entity work.mpcsm
    port map (yboolb => uiyhmirsl, byrwwjcef => khzvtf, ptxnwv => uztqbzo, ur => rlzampwl);
  imwzn : entity work.yjrxus
    port map (mjebhn => ebasacl, jmaftqk => a);
  
  -- Single-driven assignments
  rykxyxfjk <= 40041;
  uztqbzo <= yzckuxb;
  
  -- Multi-driven assignments
  wbpayuwt <= wbpayuwt;
  uiyhmirsl <= wbpayuwt;
  ebasacl <= ebasacl;
end onjx;



-- Seed after: 9047233429377137006,8067602802092121131
