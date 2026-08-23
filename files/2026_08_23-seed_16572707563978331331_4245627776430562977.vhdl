-- Seed: 16572707563978331331,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity zcmpaxrw is
  port ( sls : in bit_vector(0 downto 2)
  ; oyvwttmjx : inout integer
  ; ojgfpuryu : in bit_vector(0 downto 1)
  ; ckeyuprnp : out std_logic_vector(0 downto 3)
  );
end zcmpaxrw;

architecture ff of zcmpaxrw is
  
begin
  -- Single-driven assignments
  oyvwttmjx <= oyvwttmjx;
  
  -- Multi-driven assignments
  ckeyuprnp <= ckeyuprnp;
end ff;

entity kzs is
  port (srkujicwba : buffer time_vector(0 to 4); recryrljq : inout time);
end kzs;

library ieee;
use ieee.std_logic_1164.all;

architecture cftryxvpf of kzs is
  signal hlopkqio : std_logic_vector(0 downto 3);
  signal tcxb : integer;
  signal ialdokzuq : integer;
  signal sy : bit_vector(0 downto 2);
  signal qwbbqilym : std_logic_vector(0 downto 3);
  signal sbborz : bit_vector(0 downto 1);
  signal ysndhre : integer;
  signal hw : bit_vector(0 downto 2);
  signal afkatorq : std_logic_vector(0 downto 3);
  signal ljqjctcaz : integer;
  signal jd : bit_vector(0 downto 1);
begin
  ewc : entity work.zcmpaxrw
    port map (sls => jd, oyvwttmjx => ljqjctcaz, ojgfpuryu => jd, ckeyuprnp => afkatorq);
  mts : entity work.zcmpaxrw
    port map (sls => hw, oyvwttmjx => ysndhre, ojgfpuryu => sbborz, ckeyuprnp => qwbbqilym);
  qcvkuupndd : entity work.zcmpaxrw
    port map (sls => sy, oyvwttmjx => ialdokzuq, ojgfpuryu => jd, ckeyuprnp => afkatorq);
  tlps : entity work.zcmpaxrw
    port map (sls => sy, oyvwttmjx => tcxb, ojgfpuryu => jd, ckeyuprnp => hlopkqio);
  
  -- Multi-driven assignments
  qwbbqilym <= (others => '0');
  qwbbqilym <= (others => '0');
end cftryxvpf;



-- Seed after: 16888903166409081915,4245627776430562977
