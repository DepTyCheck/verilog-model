-- Seed: 7433406917422254235,8891552411914730853

entity ccv is
  port (xakxczk : buffer real; nf : out real; l : inout real);
end ccv;

architecture vkkvlsow of ccv is
  
begin
  -- Single-driven assignments
  l <= l;
end vkkvlsow;

entity nbyt is
  port (shzyonfn : in time_vector(3 to 1); riasp : in character);
end nbyt;

architecture qjnrqffru of nbyt is
  signal xqis : real;
  signal ncykfy : real;
  signal gja : real;
  signal sxbnv : real;
  signal zhq : real;
  signal djlmcy : real;
begin
  eyqyp : entity work.ccv
    port map (xakxczk => djlmcy, nf => zhq, l => sxbnv);
  okuhzvbhtp : entity work.ccv
    port map (xakxczk => gja, nf => ncykfy, l => xqis);
end qjnrqffru;

library ieee;
use ieee.std_logic_1164.all;

entity lamhrhmd is
  port (bgeyyr : out bit; pplcbuxhub : out real; s : inout std_logic; rhznsokkqv : in time);
end lamhrhmd;

architecture bpr of lamhrhmd is
  signal yysauqu : character;
  signal hs : time_vector(3 to 1);
begin
  svjjdza : entity work.nbyt
    port map (shzyonfn => hs, riasp => yysauqu);
  
  -- Single-driven assignments
  pplcbuxhub <= 3_0.2;
  yysauqu <= 'i';
  
  -- Multi-driven assignments
  s <= s;
  s <= 'H';
end bpr;



-- Seed after: 16094387398426936298,8891552411914730853
