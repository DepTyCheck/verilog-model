-- Seed: 4760891414655324296,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity xtft is
  port ( olvyqquoxh : inout std_logic_vector(4 to 1)
  ; mzcgyleitx : buffer std_logic
  ; nmaylxox : linkage boolean
  ; tmzroapvo : in std_logic_vector(2 to 1)
  );
end xtft;

architecture elhxlr of xtft is
  
begin
  -- Multi-driven assignments
  olvyqquoxh <= tmzroapvo;
  olvyqquoxh <= (others => '0');
end elhxlr;

entity na is
  port (evsjpynad : linkage time);
end na;

library ieee;
use ieee.std_logic_1164.all;

architecture jyifyv of na is
  signal ghhejhwxfz : boolean;
  signal yu : std_logic_vector(4 to 1);
  signal w : boolean;
  signal g : std_logic_vector(4 to 1);
  signal xpzkxallf : std_logic_vector(2 to 1);
  signal ckf : boolean;
  signal dxnlqjovkn : std_logic_vector(2 to 1);
  signal jmefv : std_logic_vector(2 to 1);
  signal e : boolean;
  signal rebwrovehp : std_logic;
  signal ww : std_logic_vector(2 to 1);
begin
  yfuesjms : entity work.xtft
    port map (olvyqquoxh => ww, mzcgyleitx => rebwrovehp, nmaylxox => e, tmzroapvo => jmefv);
  chyow : entity work.xtft
    port map (olvyqquoxh => dxnlqjovkn, mzcgyleitx => rebwrovehp, nmaylxox => ckf, tmzroapvo => xpzkxallf);
  hudjfe : entity work.xtft
    port map (olvyqquoxh => g, mzcgyleitx => rebwrovehp, nmaylxox => w, tmzroapvo => dxnlqjovkn);
  krt : entity work.xtft
    port map (olvyqquoxh => yu, mzcgyleitx => rebwrovehp, nmaylxox => ghhejhwxfz, tmzroapvo => ww);
  
  -- Multi-driven assignments
  dxnlqjovkn <= "";
  rebwrovehp <= rebwrovehp;
  jmefv <= "";
  ww <= (others => '0');
end jyifyv;

entity vqihh is
  port (lioi : inout time_vector(4 downto 3));
end vqihh;

library ieee;
use ieee.std_logic_1164.all;

architecture ahmztz of vqihh is
  signal wqymnwxd : boolean;
  signal llodkpwz : std_logic;
  signal yzqztvl : std_logic_vector(2 to 1);
begin
  bgqzskb : entity work.xtft
    port map (olvyqquoxh => yzqztvl, mzcgyleitx => llodkpwz, nmaylxox => wqymnwxd, tmzroapvo => yzqztvl);
  
  -- Single-driven assignments
  lioi <= (4_3_0.2_0_4_4_2 ns, 2.0_2_4_4_3 us);
end ahmztz;



-- Seed after: 7477280369583202234,2511821214772927453
