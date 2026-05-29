-- Seed: 6705620597014822862,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity kgnx is
  port (wj : out boolean_vector(1 downto 1); bvs : inout time_vector(3 downto 0); ukckpac : out bit; c : inout std_logic_vector(3 to 4));
end kgnx;



architecture isvrb of kgnx is
  
begin
  
end isvrb;

library ieee;
use ieee.std_logic_1164.all;

entity urqmgg is
  port (ysszbs : in std_logic; ngo : inout time; mhf : inout std_logic_vector(3 downto 1); wckufez : out std_logic_vector(2 to 3));
end urqmgg;



architecture gdugp of urqmgg is
  signal utlpnlzwqp : bit;
  signal khaugwkez : time_vector(3 downto 0);
  signal txlmm : boolean_vector(1 downto 1);
begin
  vejv : entity work.kgnx
    port map (wj => txlmm, bvs => khaugwkez, ukckpac => utlpnlzwqp, c => wckufez);
end gdugp;

library ieee;
use ieee.std_logic_1164.all;

entity cokewn is
  port ( imttwsr : linkage bit
  ; fsteo : buffer std_logic_vector(0 downto 4)
  ; tywxhgk : buffer std_logic_vector(2 to 4)
  ; tlwsppoud : buffer integer_vector(3 to 1)
  );
end cokewn;

library ieee;
use ieee.std_logic_1164.all;

architecture tndnbly of cokewn is
  signal qvldwxjq : std_logic_vector(3 to 4);
  signal h : bit;
  signal mbapq : time_vector(3 downto 0);
  signal mpbpr : boolean_vector(1 downto 1);
  signal saqhx : std_logic_vector(2 to 3);
  signal dcwgp : time;
  signal fdd : std_logic;
begin
  cdo : entity work.urqmgg
    port map (ysszbs => fdd, ngo => dcwgp, mhf => tywxhgk, wckufez => saqhx);
  uyqgn : entity work.kgnx
    port map (wj => mpbpr, bvs => mbapq, ukckpac => h, c => qvldwxjq);
end tndnbly;



-- Seed after: 3251618794290334802,16715549879197889543
