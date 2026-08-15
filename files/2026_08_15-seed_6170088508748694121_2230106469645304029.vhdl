-- Seed: 6170088508748694121,2230106469645304029

entity ykwnhtdka is
  port (what : out real; d : in severity_level; qatms : buffer real_vector(4 to 0));
end ykwnhtdka;

architecture zlh of ykwnhtdka is
  
begin
  -- Single-driven assignments
  qatms <= (others => 0.0);
  what <= 2#0_1.0_0_0_0#;
end zlh;

entity tfukrtxvlu is
  port (fknmgwgnij : out real);
end tfukrtxvlu;

architecture c of tfukrtxvlu is
  signal pncqsfjb : real_vector(4 to 0);
  signal z : severity_level;
begin
  zwjezxp : entity work.ykwnhtdka
    port map (what => fknmgwgnij, d => z, qatms => pncqsfjb);
  
  -- Single-driven assignments
  z <= ERROR;
end c;



-- Seed after: 4445693888133003283,2230106469645304029
