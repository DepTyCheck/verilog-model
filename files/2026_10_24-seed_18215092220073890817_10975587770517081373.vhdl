-- Seed: 18215092220073890817,10975587770517081373

entity avoztz is
  port (efxt : inout bit_vector(2 downto 0); bgy : inout bit_vector(1 to 2); yhc : linkage character);
end avoztz;

architecture nlvcqihaq of avoztz is
  
begin
  -- Single-driven assignments
  bgy <= ('1', '0');
  efxt <= efxt;
end nlvcqihaq;

entity boxzwfjice is
  port (udsszaolr : inout bit_vector(3 to 1); pk : linkage string(4 to 2); gas : in time);
end boxzwfjice;

architecture wprcxx of boxzwfjice is
  signal nxjsc : character;
  signal slwbc : bit_vector(1 to 2);
  signal yuaprjwoml : bit_vector(2 downto 0);
  signal nicg : character;
  signal uamioa : bit_vector(1 to 2);
  signal dqgwlri : bit_vector(2 downto 0);
  signal jh : character;
  signal sbslia : bit_vector(1 to 2);
  signal zjdokjggm : bit_vector(2 downto 0);
begin
  rjxypooyec : entity work.avoztz
    port map (efxt => zjdokjggm, bgy => sbslia, yhc => jh);
  upry : entity work.avoztz
    port map (efxt => dqgwlri, bgy => uamioa, yhc => nicg);
  sgmq : entity work.avoztz
    port map (efxt => yuaprjwoml, bgy => slwbc, yhc => nxjsc);
  
  -- Single-driven assignments
  udsszaolr <= (others => '0');
end wprcxx;



-- Seed after: 13953507722941606737,10975587770517081373
