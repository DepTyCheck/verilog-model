-- Seed: 12330045173253866222,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (ocadmtqe : buffer std_logic_vector(3 to 1); lbnq : linkage std_logic; ya : buffer std_logic_vector(4 downto 4));
end r;

architecture igovzvnms of r is
  
begin
  -- Multi-driven assignments
  ya <= ya;
  ya <= (others => 'U');
  ya <= "Z";
end igovzvnms;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port ( qq : inout std_logic_vector(1 downto 0)
  ; hmsfy : out integer
  ; kcy : linkage std_logic_vector(1 downto 4)
  ; jabmg : linkage boolean_vector(2 to 4)
  );
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture ldzfjxnz of t is
  signal wf : std_logic_vector(4 downto 4);
  signal zxgtox : std_logic;
  signal hl : std_logic_vector(4 downto 4);
  signal nplzxoj : std_logic;
  signal gqgzera : std_logic_vector(3 to 1);
begin
  fsycvtkx : entity work.r
    port map (ocadmtqe => gqgzera, lbnq => nplzxoj, ya => hl);
  an : entity work.r
    port map (ocadmtqe => gqgzera, lbnq => nplzxoj, ya => hl);
  u : entity work.r
    port map (ocadmtqe => gqgzera, lbnq => zxgtox, ya => wf);
  abvznq : entity work.r
    port map (ocadmtqe => gqgzera, lbnq => zxgtox, ya => hl);
  
  -- Single-driven assignments
  hmsfy <= 2342;
  
  -- Multi-driven assignments
  zxgtox <= nplzxoj;
end ldzfjxnz;

entity olaupbsb is
  port (oafa : in boolean_vector(2 to 0));
end olaupbsb;

library ieee;
use ieee.std_logic_1164.all;

architecture x of olaupbsb is
  signal eyhsrf : std_logic_vector(4 downto 4);
  signal njm : std_logic;
  signal nmwmjt : std_logic_vector(3 to 1);
begin
  vzjtsjhtn : entity work.r
    port map (ocadmtqe => nmwmjt, lbnq => njm, ya => eyhsrf);
  
  -- Multi-driven assignments
  eyhsrf <= eyhsrf;
  njm <= 'W';
end x;



-- Seed after: 4767015680029579086,3400751927341804175
