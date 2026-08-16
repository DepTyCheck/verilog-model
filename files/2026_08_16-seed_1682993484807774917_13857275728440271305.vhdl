-- Seed: 1682993484807774917,13857275728440271305

entity yjjyyrefof is
  port (wpwxhpi : buffer integer_vector(2 downto 3); s : inout time_vector(2 to 4));
end yjjyyrefof;

architecture kg of yjjyyrefof is
  
begin
  -- Single-driven assignments
  s <= s;
end kg;

library ieee;
use ieee.std_logic_1164.all;

entity cosdhu is
  port (fzqsi : out std_logic; sntc : buffer real; abuikozslq : buffer std_logic; dsstqgwjb : in std_logic);
end cosdhu;

architecture pbzpjtfxd of cosdhu is
  signal xlcfqtbh : time_vector(2 to 4);
  signal xpjyfgeamj : integer_vector(2 downto 3);
  signal gkhpom : time_vector(2 to 4);
  signal gx : integer_vector(2 downto 3);
  signal icbjixmov : time_vector(2 to 4);
  signal jsourlbax : integer_vector(2 downto 3);
begin
  k : entity work.yjjyyrefof
    port map (wpwxhpi => jsourlbax, s => icbjixmov);
  wphun : entity work.yjjyyrefof
    port map (wpwxhpi => gx, s => gkhpom);
  gvdbxkbbo : entity work.yjjyyrefof
    port map (wpwxhpi => xpjyfgeamj, s => xlcfqtbh);
  
  -- Single-driven assignments
  sntc <= sntc;
end pbzpjtfxd;

entity hkxnvb is
  port (fqgr : inout bit_vector(3 downto 3); kwxhjtpssi : inout integer; qbk : in real; arqnnuyg : linkage bit);
end hkxnvb;

library ieee;
use ieee.std_logic_1164.all;

architecture uae of hkxnvb is
  signal acrpfpvoui : std_logic;
  signal vzogze : real;
  signal qlddjzxbi : std_logic;
begin
  sxydp : entity work.cosdhu
    port map (fzqsi => qlddjzxbi, sntc => vzogze, abuikozslq => acrpfpvoui, dsstqgwjb => acrpfpvoui);
  
  -- Single-driven assignments
  kwxhjtpssi <= 1;
  fqgr <= (others => '0');
  
  -- Multi-driven assignments
  qlddjzxbi <= 'H';
  qlddjzxbi <= qlddjzxbi;
  qlddjzxbi <= qlddjzxbi;
end uae;



-- Seed after: 14706842683106380124,13857275728440271305
