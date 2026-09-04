-- Seed: 17199110106168843940,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity idca is
  port (tktgeqvirk : in std_logic; tutemghkc : in time; ldf : in time; lynvqdqhwp : buffer std_logic);
end idca;

architecture qjdzmznmc of idca is
  
begin
  -- Multi-driven assignments
  lynvqdqhwp <= '0';
  lynvqdqhwp <= '1';
end qjdzmznmc;

entity fnrxdtrdja is
  port (boisgy : buffer real);
end fnrxdtrdja;

library ieee;
use ieee.std_logic_1164.all;

architecture qax of fnrxdtrdja is
  signal edaeejlnra : time;
  signal djy : time;
  signal stlss : std_logic;
  signal nc : time;
  signal adluclgex : std_logic;
begin
  tcpjwxbx : entity work.idca
    port map (tktgeqvirk => adluclgex, tutemghkc => nc, ldf => nc, lynvqdqhwp => stlss);
  zpob : entity work.idca
    port map (tktgeqvirk => stlss, tutemghkc => djy, ldf => nc, lynvqdqhwp => adluclgex);
  nwweoia : entity work.idca
    port map (tktgeqvirk => adluclgex, tutemghkc => edaeejlnra, ldf => nc, lynvqdqhwp => adluclgex);
  
  -- Multi-driven assignments
  adluclgex <= '-';
  adluclgex <= 'H';
  adluclgex <= '1';
end qax;



-- Seed after: 6430523451000731428,4404421571376382767
