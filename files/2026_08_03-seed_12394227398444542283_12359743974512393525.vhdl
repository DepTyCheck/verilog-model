-- Seed: 12394227398444542283,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity kbpsdhgnuo is
  port (jzzvclfr : linkage integer_vector(2 downto 3); zvtqv : in time; g : out time; uim : in std_logic_vector(3 downto 4));
end kbpsdhgnuo;

architecture yxvey of kbpsdhgnuo is
  
begin
  -- Single-driven assignments
  g <= 8#772.523# ps;
end yxvey;

library ieee;
use ieee.std_logic_1164.all;

entity mbj is
  port (kcy : in boolean; q : out boolean; uqkept : linkage std_logic);
end mbj;

library ieee;
use ieee.std_logic_1164.all;

architecture qcbqzq of mbj is
  signal zsze : std_logic_vector(3 downto 4);
  signal snbomnxha : time;
  signal beshs : integer_vector(2 downto 3);
  signal bnf : std_logic_vector(3 downto 4);
  signal ufyibx : time;
  signal fsef : time;
  signal wonxt : integer_vector(2 downto 3);
begin
  anqpeokg : entity work.kbpsdhgnuo
    port map (jzzvclfr => wonxt, zvtqv => fsef, g => ufyibx, uim => bnf);
  sffwxiva : entity work.kbpsdhgnuo
    port map (jzzvclfr => beshs, zvtqv => fsef, g => snbomnxha, uim => zsze);
  
  -- Single-driven assignments
  q <= FALSE;
  
  -- Multi-driven assignments
  bnf <= bnf;
  bnf <= (others => '0');
  bnf <= (others => '0');
end qcbqzq;

entity zvhvj is
  port (ssfm : buffer bit);
end zvhvj;

library ieee;
use ieee.std_logic_1164.all;

architecture mlc of zvhvj is
  signal awqtckn : std_logic_vector(3 downto 4);
  signal hwrzwez : time;
  signal prtcrdik : integer_vector(2 downto 3);
  signal jvxy : std_logic_vector(3 downto 4);
  signal xohnrfkjpc : time;
  signal zuuiwzkv : integer_vector(2 downto 3);
begin
  sdtvxb : entity work.kbpsdhgnuo
    port map (jzzvclfr => zuuiwzkv, zvtqv => xohnrfkjpc, g => xohnrfkjpc, uim => jvxy);
  qgwvfzc : entity work.kbpsdhgnuo
    port map (jzzvclfr => prtcrdik, zvtqv => hwrzwez, g => hwrzwez, uim => awqtckn);
  
  -- Single-driven assignments
  ssfm <= '1';
  
  -- Multi-driven assignments
  awqtckn <= jvxy;
  jvxy <= (others => '0');
end mlc;



-- Seed after: 10930058385888782442,12359743974512393525
