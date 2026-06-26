-- Seed: 9906236884776443616,12011142928354116943

entity t is
  port (ufqj : linkage time; rcvwdyrva : buffer real; jbkfipytf : in string(1 downto 5); jaxbohu : out bit);
end t;

architecture xlobcjgi of t is
  
begin
  -- Single-driven assignments
  jaxbohu <= '1';
  rcvwdyrva <= 2#0_1_0.0_1_0_1_1#;
end xlobcjgi;

library ieee;
use ieee.std_logic_1164.all;

entity ikyewhdik is
  port (boxevmzd : out time; oxvmqmpcr : in time; flu : out std_logic_vector(0 downto 3));
end ikyewhdik;

architecture vyc of ikyewhdik is
  signal saew : bit;
  signal xcfmcdqeb : string(1 downto 5);
  signal nmugybdmq : real;
begin
  jdhommhku : entity work.t
    port map (ufqj => boxevmzd, rcvwdyrva => nmugybdmq, jbkfipytf => xcfmcdqeb, jaxbohu => saew);
  
  -- Multi-driven assignments
  flu <= (others => '0');
end vyc;

library ieee;
use ieee.std_logic_1164.all;

entity hpsfhfsbvm is
  port (bjxttfw : out std_logic_vector(0 to 1); i : out bit);
end hpsfhfsbvm;

library ieee;
use ieee.std_logic_1164.all;

architecture efuzesiotl of hpsfhfsbvm is
  signal nkovbbjbf : std_logic_vector(0 downto 3);
  signal qhwbcecoha : time;
  signal uhqfrjwt : string(1 downto 5);
  signal uhrzmsikxo : real;
  signal hbq : time;
begin
  gqnoksf : entity work.t
    port map (ufqj => hbq, rcvwdyrva => uhrzmsikxo, jbkfipytf => uhqfrjwt, jaxbohu => i);
  ghcplwy : entity work.ikyewhdik
    port map (boxevmzd => qhwbcecoha, oxvmqmpcr => qhwbcecoha, flu => nkovbbjbf);
  
  -- Single-driven assignments
  uhqfrjwt <= "";
  
  -- Multi-driven assignments
  bjxttfw <= ('-', 'U');
  bjxttfw <= "X-";
end efuzesiotl;



-- Seed after: 8716020602513780052,12011142928354116943
