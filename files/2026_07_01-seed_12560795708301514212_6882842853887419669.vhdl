-- Seed: 12560795708301514212,6882842853887419669

entity fpucycjje is
  port (ttknwpbry : in string(5 downto 4); tiyvypu : in real_vector(4 to 0));
end fpucycjje;

architecture seqoqfnop of fpucycjje is
  
begin
  
end seqoqfnop;

entity qvojr is
  port (xdij : inout integer);
end qvojr;

architecture uuo of qvojr is
  signal tap : real_vector(4 to 0);
  signal zlo : string(5 downto 4);
  signal wr : real_vector(4 to 0);
  signal xwdyltp : string(5 downto 4);
begin
  zictzjbxbo : entity work.fpucycjje
    port map (ttknwpbry => xwdyltp, tiyvypu => wr);
  xnxioko : entity work.fpucycjje
    port map (ttknwpbry => zlo, tiyvypu => tap);
  zjfjudsknm : entity work.fpucycjje
    port map (ttknwpbry => xwdyltp, tiyvypu => tap);
  
  -- Single-driven assignments
  xdij <= 2_0_4_3;
  xwdyltp <= "ni";
end uuo;



-- Seed after: 12425269546787505224,6882842853887419669
